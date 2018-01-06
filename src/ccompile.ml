open Lambda
open Types
open Primitive
open Ccode
open Asttypes
open Typecollect

exception Undefined
exception NotSupported of string

let fresh_var name = CVar (Ident.create name)

let rec zip xs ys = match (xs, ys) with
  | x::xs, y::ys -> (x, y) :: zip xs ys
  | [], _ -> []
  | _, [] -> []

let unpack_loc loc =
  let { Location.loc_start } = loc in
  let { Lexing.pos_fname = fname; Lexing.pos_lnum = lnum } = loc_start in
  (lnum, fname)

let comp_code lambda (types, externals) =
  (* Note: these lists are in reverse order to make cons's constant time *)
  let preamble = ref ([] : cstatement list) in
  let funcs = ref ([] : cfunc list) in
  let stmts = ref ([] : cstatement list) in
  let curr_loc = ref Location.none in

  (* Dirty shadowing to avoid referring to the types hash all the time *)
  let set_type v t = match v with
    | CVar i -> set_type types i t
    | CGlobalVar s -> ()
  in
  let get_type = function
    | CVar i -> get_type types i
    | CGlobalVar s -> CPointer CVoid
  in

  (* Helper function for adding statements *)
  let add s = stmts := s :: !stmts in

  (* Shortcut for declaring a temporary variable for an expression *)
  let decl_assign ?(name="temp") exp ty =
    let var = fresh_var name in
    set_type var ty;
    add (CDecl (var, ty));
    add (CAssign (CIdent var, exp));
    var
  in

  let name_exp var_name = CIdent (CGlobalVar var_name) in

  (* Block helpers *)
  let block_field var n =
    COffset (CCall (name_exp "GET_BLOCK", [CIdent var]), n + 1)
  in
  let block_tag exp =
    CCall (name_exp "GET_INT",
           [CDeref (CCall (name_exp "GET_BLOCK", [exp]))])
  in

  (* Cast helper to deal with the different cases under which casting can occur *)
  let rec cast old_ty new_ty exp =
    if old_ty = new_ty then exp
    else if old_ty = CValue then
      begin
        (* unbox CValue and cast to new type *)
        match new_ty with
          | CUInt
          | CInt -> CCall (name_exp "GET_INT", [exp])
          | CFloat -> CCall (name_exp "GET_FLOAT", [exp])
          | CStr -> CCall (name_exp "GET_STRING", [exp])
          | CFuncPointer _ -> CCast (CCall (name_exp "GET_FUNC", [exp]), new_ty)
          | _ -> failwith ("bad type to unbox from: " ^
                           Cprint.sprint Cprint.print_ctype new_ty)
      end
    else if new_ty = CValue then
      begin
        (* box value and cast to new type *)
        match old_ty with
          | CUInt
          | CInt -> CCall (name_exp "BOX_INT", [exp])
          | CFloat -> CCall (name_exp "BOX_FLOAT", [exp])
          | CStr -> CCall (name_exp "BOX_STRING", [exp])
          | CFuncPointer _ -> CCall (name_exp "BOX_FUNC", [exp])
          | CPointer CVoid -> (* Change null types to just ints? *)
              CCall (name_exp "BOX_INT", [cast old_ty CInt exp])
          | _ -> failwith ("bad type to box: " ^
                           Cprint.sprint Cprint.print_ctype old_ty)
      end
    else
      begin
        (* todo: currently just a dirty cast *)
        Printf.printf "Warning: dirty cast between %s and %s\n"
          (Cprint.sprint Cprint.print_ctype old_ty)
          (Cprint.sprint Cprint.print_ctype new_ty);
        CCast (exp, new_ty)
      end

  and assign (target_var, target_ty) (old_exp, old_ty) =
    add @@ CAssign (target_var, cast old_ty target_ty old_exp)

  and unify_types t1 t2 =
    if t1 = t2 then t1
    else if t1 = CValue || t2 = CValue then CValue
    else if t1 = CPointer CVoid then t2
    else if t2 = CPointer CVoid then t1
    else failwith (Printf.sprintf "cannot unify types %s and %s\n"
      (Cprint.sprint Cprint.print_ctype t1)
      (Cprint.sprint Cprint.print_ctype t2))

  and set_loc ev =
    let { lev_loc } = ev in
    curr_loc := lev_loc;
    let lnum, fname = unpack_loc lev_loc in
    add (CLoc (lnum, fname))

  (* Makes a new context for which stmts is now blank, executes f, and then
   * restores the old context, returning the result of f and the new context
   *)
  and with_context ?(init_block=[]) f =
    let curr_stmts = !stmts in
    stmts := List.rev init_block;
    let res = f () in
    let nstmts = !stmts in
    stmts := curr_stmts;
    (res, List.rev nstmts)

  (* Compiles a constant. *)
  and comp_constant = function
    | Const_base base -> begin match base with
      | Const_int i -> (CLInt i, CInt)
      | Const_char c -> raise (NotSupported "Chars are not supported")
      | Const_string (s, _) -> (CLString s, CStr)
      | Const_float s -> (CLFloat (float_of_string s), CFloat)
      | Const_int32 i -> (CLInt (Int32.to_int i), CInt)
      | Const_int64 i -> (CLInt (Int64.to_int i), CInt)
      | Const_nativeint i -> (CLInt (Nativeint.to_int i), CInt)
    end

    | Const_immstring s -> (CLString s, CStr)
    | Const_pointer n -> (CLInt n, CPointer CVoid)

    | Const_block (tag, args) ->
        (* Just convert it into a regular Pmakeblock *)
        let lams = List.map (fun x -> Lconst x) args in
        let exp = Lprim (Pmakeblock (tag, Immutable, None), lams, !curr_loc) in
        let var = comp_expr exp in
        (CIdent var, get_type var)

    | c -> failwith "Unsupported constant"

  (* Compiles the top-level module code.
   * Similar to comp_expr, but deals with a few special structures such as
   * top-level function declarations and Pmakeblocks.
   *)
  and comp_root exp =
    match exp with
      | Levent (body, ev) ->
          set_loc ev;
          comp_root body
      | Llet (_, _, id, Lfunction { params; body }, rest) ->
          begin
            match get_type (CVar id) with
              | CFuncPointer (rt, arg_tys) ->
                  (* set types for each of the args, just in case *)
                  let args = zip (List.map (fun x -> CVar x) params) arg_tys in
                  List.fold_left (fun () (arg, ty) -> set_type arg ty) () args;
                  (* Drop into non-toplevel for function body *)
                  let (rvar, fblock) = with_context (fun () ->
                    let rvar = comp_expr body in
                    add (CReturn rvar);
                    rvar
                  ) in
                  let temp_var = fresh_var "func" in
                  funcs := {
                    return_type = rt;
                    args;
                    id = temp_var;
                    body = fblock;
                    loc = unpack_loc !curr_loc;
                  } :: !funcs;
                  preamble := CDecl (CVar id, get_type (CVar id)) :: !preamble;
                  add (CAssign (CIdent (CVar id), CRef (CIdent temp_var)))

              | _ -> failwith "type of function was not a function pointer"
          end;
          comp_root rest

      | Llet (_, _, id, arg, rest) ->
          (* Declare a global constant *)
          let ty = get_type (CVar id) in
          preamble := CDecl (CVar id, ty) :: !preamble;
          let res = comp_expr arg in
          assign (CIdent (CVar id), ty) (CIdent res, get_type res);
          comp_root rest

      | Lletrec (decls, body) ->
          (* Dirty hack - does this always work? *)
          let expr = List.fold_right (fun (id, arg) rest ->
            Llet(Strict, Pgenval, id, arg, rest)) decls body in
          comp_root expr

      | Lsequence (e1, e2) ->
          comp_expr e1 |> ignore;
          comp_root e2

      | Lprim (Pmakeblock(_, Immutable, _), args, _) ->
          (* module makeblock! *)
          let var = fresh_var "module" in
          let ty = CValue in
          preamble := CDecl (var, ty) :: !preamble;
          set_type var ty;
          let res = comp_expr exp in
          assign (CIdent var, ty) (CIdent res, get_type res);
          var

      | _ -> comp_expr exp

  (* Compiles an expression.
   * Always returns an identifier for which the result of the expression
   * can be used.
   *
   * Invariant: the type of each variable is always known and present in
   * the types hash table when it is returned from a comp_expr call.
   *)
  and comp_expr exp =
    match exp with
      | Lvar id -> CVar id

      | Lconst const ->
          let (exp, ty) = comp_constant const in
          decl_assign ~name:"const" exp ty

      | Lapply { ap_func; ap_args; ap_loc } ->
          curr_loc := ap_loc;
          let fvar = comp_expr ap_func in
          let args = List.fold_left
            (fun args exp -> let arg = comp_expr exp in arg :: args)
            [] ap_args |> List.rev in

          (* Assert no partial application *)
          begin
            match get_type fvar with
              | CFuncPointer (rt, arg_tys) ->
                  if List.length arg_tys <> List.length args
                  then failwith "Partial application is not supported"
                  else

                    let exp = CCall
                      (CIdent fvar, List.map (fun x -> CIdent x) args) in
                    decl_assign ~name:"call_result" exp rt

              | CPointer CVoid ->
                  Printf.printf "Warning: void* call\n";
                  let exp = CCall
                    (CIdent fvar, List.map (fun x -> CIdent x) args) in
                  decl_assign ~name:"call_result" exp (CPointer CVoid)

              (* TODO: deal with cvalues as function pointers *)

              | _ -> failwith
                ((Cprint.sprint Cprint.print_cident fvar) ^ " was not a function pointer")
          end

      | Lfunction _ -> failwith "closures unsupported!"

      (* [ let x = M in N ] =
       *
       * [M]
       * decl(temp);
       * {
       *   decl(x);
       *   x = var(M);
       *   [N]
       *   temp = var(N)
       * }
       * -> temp
       *)
      | Llet (_, _, id, arg, body) ->
          let arg_var = comp_expr arg in
          let arg_ty = get_type (CVar id) in
          let temp = fresh_var "let_return" in
          let (rvar, lblock) = with_context (fun () ->
            add (CDecl (CVar id, arg_ty));
            assign (CIdent (CVar id), arg_ty) (CIdent arg_var, get_type arg_var);
            let rvar = comp_expr body in
            assign (CIdent temp, get_type rvar) (CIdent rvar, get_type rvar);
            rvar
          ) in
          let rty = get_type rvar in
          set_type temp rty;
          add (CDecl (temp, rty));
          add (CBlock (lblock));
          temp

      | Lletrec (decls, body) ->
          let expr = List.fold_right (fun (id, arg) rest ->
            Llet(Strict, Pgenval, id, arg, rest)) decls body in
          comp_expr expr

      | Lprim (p, ls, loc) ->
          curr_loc := loc;
          comp_prim p ls

      (* Switch statements contain some which switch on ints, and some that
       * switch on tags
       *
       * Convert this into two sets of switch statements, one for if the value
       * is an int, and the other for if the value is a block
       *)
      | Lswitch (lam, sw) ->
          let var = comp_expr lam in
          let var = cast (get_type var) CValue (CIdent var) in
          let condition = CCall (name_exp "IS_INT", [var]) in
          let rvar = fresh_var "switch_return" in
          let fail_label = Ident.unique_name (Ident.create "switch_fail") in

          (* Messy threading - compile half way, don't assign yet,
           * then figure out the return type, and then add the assignments
           * back in
           *)
          let int_blocks = List.map (fun (i, lam) ->
            (i, with_context (fun () -> comp_expr lam))) sw.sw_consts in
          let tag_blocks = List.map (fun (i, lam) ->
            (i, with_context (fun () -> comp_expr lam))) sw.sw_blocks in
          let fail_block = match sw.sw_failaction with
            | Some lam ->
                Some (with_context (fun () ->
                  add (CLabel fail_label);
                  comp_expr lam))
            | None -> None
          in

          (* Determine type! *)
          (* No composition operator in OCaml why *)
          let rty = List.fold_left unify_types (CPointer CVoid)
            (List.map (fun (_, (v, _)) -> get_type v) int_blocks) in
          let rty = List.fold_left unify_types rty
            (List.map (fun (_, (v, _)) -> get_type v) tag_blocks) in
          let rty = match fail_block with
            | Some (v, _) -> unify_types rty (get_type v)
            | None -> rty
          in

          (* Now add appropriate assignments back to each block *)
          let int_blocks = List.map (fun (i, (v, b)) ->
            let (_, b) = with_context ~init_block:b (fun () ->
              assign (CIdent rvar, rty) (CIdent v, get_type v); v) in
            (i, b)) int_blocks in
          let tag_blocks = List.map (fun (i, (v, b)) ->
            let (_, b) = with_context ~init_block:b (fun () ->
              assign (CIdent rvar, rty) (CIdent v, get_type v); v) in
            (i, b)) tag_blocks in
          let fail_block = match fail_block with
            | Some (v, b) ->
                let (_, b) = with_context ~init_block:b (fun () ->
                  assign (CIdent rvar, rty) (CIdent v, get_type v); v) in
                Some b
            | None -> None
          in

          (* Construct the switch statements *)
          let fail_goto = match fail_block with
            | Some _ -> Some [CGoto fail_label]
            | None -> None
          in
          let int_switch = CSwitch
            (CCall (name_exp "GET_INT", [var]), int_blocks, fail_goto) in
          let tag_switch = CSwitch
            (block_tag var, tag_blocks, fail_goto) in

          (* Construct the if statement *)
          let ifstmt = CIfElse (condition, [int_switch],
            match fail_block with
              | Some b -> [CIfElse (CLInt 1, [tag_switch], b)]
              | None -> [tag_switch]
            ) in

          add (CDecl (rvar, rty));
          add ifstmt;
          rvar

      (* [ if M then N else O ] =
       *
       * [M]
       * decl(temp);
       * if (var(M)) {
       *   [N]
       *   temp = var(N);
       * } else {
       *   [O]
       *   temp = var(O);
       * }
       * -> temp
       *)
      | Lifthenelse (i, t, e) ->
          let ivar = comp_expr i in
          let iexp = cast (get_type ivar) CInt (CIdent ivar) in
          let temp = fresh_var "ifelse_return" in

          (* Do partial compilation here to figure out types of variables *)
          let (tvar, tblock) = with_context (fun () -> comp_expr t) in
          let (evar, eblock) = with_context (fun () -> comp_expr e) in

          let rty = unify_types (get_type tvar) (get_type evar) in

          (* Add the assignments into the blocks now that you know them *)
          let (_, tblock) = with_context ~init_block:tblock (fun () ->
            assign (CIdent temp, rty) (CIdent tvar, get_type tvar); tvar
          ) in
          let (_, eblock) = with_context ~init_block:eblock (fun () ->
            assign (CIdent temp, rty) (CIdent evar, get_type evar); evar
          ) in

          set_type temp rty;
          add (CDecl (temp, rty));
          add (CIfElse (iexp, tblock, eblock));
          temp

      | Lsequence (l1, l2) ->
          comp_expr l1 |> ignore;
          comp_expr l2

      | Lwhile (cond, body) ->
          let cvar = fresh_var "while_cond" in
          set_type cvar CInt;
          add (CDecl (cvar, CInt));

          let ctemp = comp_expr cond in
          assign (CIdent cvar, CInt) (CIdent ctemp, get_type ctemp);

          let (_, block) = with_context (fun () ->
            comp_expr body |> ignore;
            let ctemp = comp_expr cond in
            assign (CIdent cvar, CInt) (CIdent ctemp, get_type ctemp);
            ctemp
          ) in
          add (CWhile (CIdent cvar, block));
          decl_assign ~name:"while_return" (CLInt 0) (CPointer CVoid)

      | Lfor (id, s, e, dir, body) ->
          (* eh, let's not bother implementing for loop syntax
           * just compile it to a while loop
           *)
          let comp_op, incr_op = match dir with
            | Upto -> ("<=", "++")
            | Downto -> (">=", "--")
          in

          let svar = comp_expr s in
          let evar = comp_expr e in

          let var = CVar id in
          set_type var CInt;
          add (CDecl (var, CInt));
          assign (CIdent var, CInt) (CIdent svar, get_type svar);
          let condition = CBinOp (comp_op,
            CIdent var, cast (get_type evar) CInt (CIdent evar)) in

          let (_, block) = with_context (fun () ->
            let rvar = comp_expr body in
            add @@ CBare (CUnOp (incr_op, CIdent var));
            rvar) in
          add @@ CWhile (condition, block);
          (* for loops return unit *)
          decl_assign ~name:"for_return" (CLInt 0) (CPointer CVoid)

      | Lassign _ -> failwith "undefined"

      | Lstaticcatch (body, (id, []), catch) ->
          let (rvar, rblock) = with_context (fun () -> comp_expr body) in
          let (cvar, cblock) = with_context (fun () ->
            add (CLabel (Printf.sprintf "staticcatch_%d" id));
            comp_expr catch) in
          let var = fresh_var "catch_return" in
          let ty = unify_types (get_type rvar) (get_type cvar) in
          add (CDecl (var, ty));
          let (_, rblock) = with_context ~init_block:rblock (fun () ->
            assign (CIdent var, ty) (CIdent rvar, get_type rvar);
            rvar) in
          let (_, cblock) = with_context ~init_block:cblock (fun () ->
            assign (CIdent var, ty) (CIdent cvar, get_type cvar);
            cvar) in

          add (CIfElse (CLInt 1, rblock, cblock));
          var
      | Lstaticraise (id, []) ->
          add (CGoto (Printf.sprintf "staticcatch_%d" id));
          (* the function contract requires me to return a variable, so...
           * I'll just define a null pointer and hope dead code elimination
           * gets rid of this bit of unreachable code
           *)
          decl_assign ~name:"dead_var" (CLInt 0) (CPointer CVoid)

      | Levent (lam, ev) ->
          set_loc ev;
          comp_expr lam

      | _ -> failwith "Unsupported lambda term"

  (* Compiles a Lprimitive expression. *)
  and comp_prim prim lambdas =
    let unop t rt op e =
      let a = comp_expr e in
      decl_assign ~name:"unop_result"
        (CUnOp (op, cast (get_type a) t (CIdent a))) rt
    in

    let bop t rt op e1 e2 =
      let a = comp_expr e1 in
      let b = comp_expr e2 in
      decl_assign ~name:"binop_result"
        (CBinOp (op, cast (get_type a) t (CIdent a),
                     cast (get_type b) t (CIdent b))) rt
    in

    let int_unop = unop CInt CInt in
    let int_bop = bop CInt CInt in
    let float_unop = unop CFloat CFloat in
    let float_bop = bop CFloat CFloat in
    let float_cmp_bop = bop CFloat CInt in

    match prim, lambdas with
      | Pmakeblock (tag, _, _), contents ->
          let vars = List.map comp_expr contents in
          let blocklen = List.length vars in
          let sizeof = CBinOp ("*", CSizeOf CValue, CLInt (blocklen + 1)) in
          let malloc_expr = CCall (name_exp "MALLOC", [sizeof]) in
          let block_expr = CCall (name_exp "BOX_BLOCK", [malloc_expr]) in
          let block = decl_assign ~name:"block" block_expr CValue in
          assign (CDeref (CCall (name_exp "GET_BLOCK", [CIdent block])), CValue)
                 (CLInt tag, CInt);
          List.iteri (fun n v ->
            assign (block_field block n, CValue) (CIdent v, get_type v)) vars;
          block

      | Pidentity, [x] -> comp_expr x

      (* Ignore result and return NULL *)
      | Pignore, [x] ->
          comp_expr x |> ignore;
          let var = fresh_var "null" in
          let ty = CPointer CVoid in
          set_type var ty;
          add (CDecl (var, ty));
          assign (CIdent var, ty) (CLInt 0, ty);
          var

      | Popaque, _ -> failwith "Find out what Popaque does"

      | Pdirapply, [func;arg]
      | Prevapply, [arg;func] ->
          comp_expr (Lapply {
            ap_should_be_tailcall = false;
            ap_loc = !curr_loc;
            ap_func = func;
            ap_args = [arg];
            ap_inlined = Default_inline;
            ap_specialised = Default_specialise;
          })

      | Pgetglobal id, [] -> CGlobalVar (Ident.name id)

      | Pfield i, [lam]
      | Pfloatfield i, [lam] ->
          let e = comp_expr lam in
          let var = fresh_var "field_access" in
          let ty = CValue in
          set_type var ty;
          add (CDecl (var, ty));
          assign (CIdent var, ty) (block_field e i, CValue);
          var

      | Psetfield (i, _, _), [trg; lam] ->
          let block = comp_expr trg in
          let var = comp_expr lam in
          assign (block_field block i, CValue) (CIdent var, get_type var);
          var

      | Psequand, [e1; e2] -> int_bop "&&" e1 e2
      | Psequor, [e1; e2] -> int_bop "||" e1 e2
      | Paddint, [e1; e2] -> int_bop "+" e1 e2
      | Psubint, [e1; e2] -> int_bop "-" e1 e2
      | Pmulint, [e1; e2] -> int_bop "*" e1 e2
      | Pdivint _, [e1; e2] -> int_bop "/" e1 e2
      | Pmodint _, [e1; e2] -> int_bop "%" e1 e2
      | Pandint, [e1; e2] -> int_bop "&" e1 e2
      | Porint, [e1; e2] -> int_bop "|" e1 e2
      | Pxorint, [e1; e2] -> int_bop "^" e1 e2
      | Plslint, [e1; e2] -> int_bop "<<" e1 e2
      | Plsrint, [e1; e2] -> bop CUInt CInt ">>" e1 e2
      | Pasrint, [e1; e2] -> int_bop ">>" e1 e2
      | Pnegint, [e] -> int_unop "-" e
      | Pintcomp (Ceq), [e1; e2] -> int_bop "==" e1 e2
      | Pintcomp (Cneq), [e1; e2] -> int_bop "!=" e1 e2
      | Pintcomp (Clt), [e1; e2] -> int_bop "<" e1 e2
      | Pintcomp (Cle), [e1; e2] -> int_bop "<=" e1 e2
      | Pintcomp (Cgt), [e1; e2] -> int_bop ">" e1 e2
      | Pintcomp (Cge), [e1; e2] -> int_bop ">=" e1 e2
      | Poffsetint n, [e] ->
          let a = comp_expr e in
          let t = get_type a in
          let temp = fresh_var "offset_result" in
          set_type temp CInt;
          add (CDecl (temp, CInt));
          add (CAssign (CIdent temp,
                        CBinOp ("+", cast t CUInt (CIdent a), CLInt n)));
          temp
      | Pfloatofint, [e] ->
          let a = comp_expr e in
          let t = get_type a in
          let temp = fresh_var "float_cast" in
          set_type temp CFloat;
          add (CDecl (temp, CFloat));
          add (CAssign (CIdent temp, cast t CFloat (CIdent a)));
          temp
      | Pintoffloat, [e] ->
          let a = comp_expr e in
          let t = get_type a in
          let temp = fresh_var "int_cast" in
          set_type temp CInt;
          add (CDecl (temp, CInt));
          add (CAssign (CIdent temp, cast t CInt (CIdent a)));
          temp
      | Paddfloat, [e1; e2] -> float_bop "+" e1 e2
      | Psubfloat, [e1; e2] -> float_bop "-" e1 e2
      | Pmulfloat, [e1; e2] -> float_bop "*" e1 e2
      | Pdivfloat, [e1; e2] -> float_bop "/" e1 e2
      | Pnegfloat, [e] -> float_unop "-" e
      | Pfloatcomp (Ceq), [e1; e2] -> float_cmp_bop "==" e1 e2
      | Pfloatcomp (Cneq), [e1; e2] -> float_cmp_bop "!=" e1 e2
      | Pfloatcomp (Clt), [e1; e2] -> float_cmp_bop "<" e1 e2
      | Pfloatcomp (Cle), [e1; e2] -> float_cmp_bop "<=" e1 e2
      | Pfloatcomp (Cgt), [e1; e2] -> float_cmp_bop ">" e1 e2
      | Pfloatcomp (Cge), [e1; e2] -> float_cmp_bop ">=" e1 e2

      | Pisint, [e] ->
          let var = comp_expr e in
          decl_assign (CCall (name_exp "IS_INT", [CIdent var])) CInt

      | Pccall { prim_name; prim_arity }, args ->
          begin
            assert (List.length args = prim_arity);
            try
              let func_type = ExternHash.find externals prim_name in
              match func_type with
                | CFuncPointer (rty, arg_tys) ->
                    let args = List.map (fun (l, a) ->
                      let v = comp_expr l in
                      let t = get_type v in
                      cast t a (CIdent v)) (zip args arg_tys)
                    in
                    let exp = CCall (name_exp prim_name, args) in
                    decl_assign ~name:"extern_call" exp rty
                | _ -> failwith @@ "Nonfunction external " ^ prim_name
            with Not_found ->
              failwith @@ "External " ^ prim_name ^ " not found"
          end

      | _ -> failwith ("primitive " ^ (Printlambda.name_of_primitive prim) ^
                       " not supported")

  in
  preamble := CInclude "runtime.h" :: !preamble;
  begin
    match lambda with
      | Lprim (Psetglobal id, [lam], _) ->
          comp_root lam |> ignore
      | _ -> failwith "unexpected root"
  end;
  {
    preamble = List.rev !preamble;
    funcs = List.rev !funcs;
    main = List.rev !stmts;
  }

