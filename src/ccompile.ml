open Lambda
open Types
open Ccode
open Asttypes
open Typecollect

exception Undefined
exception NotSupported of string

let fresh_var () = Ident.create "temp"

let rec zip xs ys = match (xs, ys) with
  | x::xs, y::ys -> (x, y) :: zip xs ys
  | [], _ -> []
  | _, [] -> []

let unpack_loc loc = 
  let { Location.loc_start } = loc in
  let { Lexing.pos_fname = fname; Lexing.pos_lnum = lnum } = loc_start in
  (lnum, fname)

let comp_code lambda types =
  (* Note: these lists are in reverse order to make cons's constant time *)
  let preamble = ref ([] : cstatement list) in
  let funcs = ref ([] : cfunc list) in
  let stmts = ref ([] : cstatement list) in
  let curr_loc = ref Location.none in

  (* Dirty shadowing to avoid referring to the types hash all the time *)
  let set_type = set_type types in
  let get_type = get_type types in
  
  (* Helper function for adding statements *)
  let add s = stmts := s :: !stmts in

  (* Makes a new context for which stmts is now blank, executes f, and then
   * restores the old context, returning the result of f and the new context
   *)
  let rec with_context f =
    let curr_stmts = !stmts in
    stmts := [];
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

    | c -> failwith "Unsupported constant"

  (* Compiles an expression.
   * Always returns an identifier for which the result of the expression
   * can be used.
   *
   * Invariant: the type of each variable is always known and present in
   * the types hash table when it is returned from a comp_expr call.
   *)
  and comp_expr exp =
    match exp with
      | Lvar id -> id

      | Lconst const ->
          let var = fresh_var () in
          let (exp, ty) = comp_constant const in
          set_type var ty;
          add (CDecl (var, ty));
          add (CAssign (var, exp));
          var

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
                    
                    let exp = CCall (fvar, List.map (fun x -> CVar x) args) in
                    let var = fresh_var () in
                    set_type var rt;
                    add (CDecl (var, rt));
                    add (CAssign (var, exp));
                    var

              | _ -> failwith "fvar was not a function pointer"
          end

      | Lfunction _ -> failwith "bare Lfunction"

      | Llet (_, _, id, Lfunction { params; body }, rest) ->
          let (rvar, fblock) = with_context (fun () ->
            let rvar = comp_expr body in
            add (CReturn rvar);
            rvar
          ) in
          begin
            match get_type id with
              | CFuncPointer (rt, arg_tys) ->
                  funcs := {
                    return_type = rt;
                    args = zip params arg_tys;
                    id;
                    body = fblock;
                    loc = unpack_loc !curr_loc;
                  } :: !funcs

              | _ -> failwith "type of function was not a function pointer"
          end;
          comp_expr rest
      
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
          let arg_ty = get_type id in
          let temp = fresh_var () in
          let (rvar, lblock) = with_context (fun () ->
            add (CDecl (id, arg_ty));
            add (CAssign (id, CVar arg_var));
            let rvar = comp_expr body in
            add (CAssign (temp, CVar rvar));
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

      | Lswitch _ -> failwith "undefined"

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
          let temp = fresh_var () in

          let (tvar, tblock) = with_context (fun () ->
            let tvar = comp_expr t in
            add (CAssign (temp, CVar tvar));
            tvar
          ) in
          let rty = get_type tvar in

          let (evar, eblock) = with_context (fun () ->
            let evar = comp_expr e in
            add (CAssign (temp, CVar evar));
            evar
          ) in

          set_type temp rty;
          add (CDecl (temp, rty));
          add (CIfElse (CVar ivar, tblock, eblock));
          temp
      
      | Lsequence (l1, l2) ->
          comp_expr l1 |> ignore;
          comp_expr l2

      | Lwhile (l, x) -> failwith "undefined"

      | Lfor _ -> failwith "undefined"

      | Lassign _ -> failwith "undefined"

      | Levent (lam, ev) ->
        let { lev_loc } = ev in
        curr_loc := lev_loc;
        let lnum, fname = unpack_loc lev_loc in
        add (CLoc (lnum, fname));
        comp_expr lam

      | _ -> failwith "Unsupported lambda term"

  (* Compiles a Lprimitive expression. *)
  and comp_prim prim lambdas =
    let unop t rt op e =
      let a = comp_expr e in
      let temp = fresh_var () in
      set_type temp rt;
      add (CDecl (temp, rt));
      add (CAssign (temp, CUnOp (op, CCast (CVar a, t))));
      temp
    in

    let bop t rt op e1 e2 =
      let a = comp_expr e1 in
      let b = comp_expr e2 in
      let temp = fresh_var () in
      set_type temp rt;
      add (CDecl (temp, rt));
      add (CAssign (temp, CBinOp (op, CCast (CVar a, t), CCast (CVar b, t))));
      temp
    in

    let int_unop = unop CInt CInt in
    let int_bop = bop CInt CInt in
    let float_unop = unop CFloat CFloat in
    let float_bop = bop CFloat CFloat in
    let float_cmp_bop = bop CFloat CInt in

    match prim, lambdas with
    (* Just ignore for now! *)
    | Psetglobal _, _ -> comp_expr (List.hd lambdas)
    | Pmakeblock _, _ ->
        let var = fresh_var () in
        let ty = CPointer CVoid in
        set_type var ty;
        var

    (* TODO: compile other primitives *)
    | Pidentity, [x] -> comp_expr x

    (* Ignore result and return NULL *)
    | Pignore, [x] ->
        comp_expr x |> ignore;
        let var = fresh_var() in
        let ty = CPointer CVoid in
        set_type var ty;
        add (CDecl (var, ty));
        add (CAssign (var, CLInt 0));
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

    | Pgetglobal id, [] -> failwith "Pgetglobal is not supported"

    | Pfield i, [lam]
    | Pfloatfield i, [lam] ->
        failwith "Pfield is not supported"

    | Psetfield _, [lam] ->
        failwith "Psetfield is not supported"

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
        let temp = fresh_var () in
        set_type temp CInt;
        add (CDecl (temp, CInt));
        add (CAssign (temp, CBinOp ("+", CCast (CVar a, CInt), CLInt n)));
        temp
    | Pfloatofint, [e] ->
        let a = comp_expr e in
        let temp = fresh_var () in
        set_type temp CFloat;
        add (CDecl (temp, CFloat));
        add (CAssign (temp, CCast (CVar a, CFloat)));
        temp
    | Pintoffloat, [e] ->
        let a = comp_expr e in
        let temp = fresh_var () in
        set_type temp CInt;
        add (CDecl (temp, CInt));
        add (CAssign (temp, CCast (CVar a, CInt)));
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


    | _ -> failwith ("primitive " ^ (Printlambda.name_of_primitive prim) ^
                     "not supported")

  in
  comp_expr lambda |> ignore;
  {
    preamble = List.rev !preamble;
    funcs = List.rev !funcs;
    main = List.rev !stmts;
  }

