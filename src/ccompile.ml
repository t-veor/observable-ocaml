open Lambda
open Types
open Primitive
open Ccode
open Asttypes
open Typecollect

exception Undefined
exception NotSupported of string

let fresh_var name = CVar (Ident.create name)

(* Haskell-like list processing functions that are somehow missing from the
 * standard library
 *)
let rec zip xs ys = match (xs, ys) with
  | x::xs, y::ys -> (x, y) :: zip xs ys
  | [], _ | _, [] -> []

let rec init xs = match xs with
  | [] | [_] -> []
  | x :: xs -> x :: init xs

let split n xs =
  let rec helper xs ys n = match n, xs with
    | 0, _ | _, [] -> (List.rev ys, xs)
    | n, x :: xs -> helper xs (x :: ys) (n - 1)
  in helper xs [] n

let enumerate n f =
  let rec helper m =
    if m < n then f m :: helper (m + 1)
    else []
  in helper 0

let repeat x n =
  let rec helper acc = function
    | 0 -> acc
    | m -> helper (x :: acc) (m - 1)
  in helper [] n

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
    | CGlobalVar s -> CValue
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

  (* closure helpers *)
  (* Get the nth argument in the current closure.
   *)
  let closure_data closure_var n =
    COffset (CArrow (CIdent closure_var, "args"), CLInt n)
  in

  let closure_malloc n =
    let size = CBinOp ("+", CSizeOf CClosureT,
      CBinOp ("*", CSizeOf CAnyType, CLInt n)) in
    CCall (name_exp "MALLOC", [size])
  in

  let pack ty exp =
    match ty with
      | CAnyType -> exp
      | CUInt -> CCall (name_exp "FROM_UINT", [exp])
      | CInt -> CCall (name_exp "FROM_INT", [exp])
      | CFloat -> CCall (name_exp "FROM_FLOAT", [exp])
      | CStr -> CCall (name_exp "FROM_STR", [exp])
      | CClosure _ -> CCall (name_exp "FROM_CLOSURE", [exp])
      | CValue -> CCall (name_exp "FROM_VALUE", [exp])
      | CFuncPointer _ -> CCast (CCall (name_exp "FROM_FUNC", [exp]), ty)
      | _ -> failwith ("bad type to pack: " ^
                       Cprint.sprint Cprint.print_ctype ty)
  in

  let unpack ty exp =
    match ty with
      | CAnyType -> exp
      | CUInt -> CCall (name_exp "TO_UINT", [exp])
      | CInt -> CCall (name_exp "TO_INT", [exp])
      | CFloat -> CCall (name_exp "TO_FLOAT", [exp])
      | CStr -> CCall (name_exp "TO_STR", [exp])
      | CClosure _ -> CCall (name_exp "TO_CLOSURE", [exp])
      | CValue -> CCall (name_exp "TO_VALUE", [exp])
      | CFuncPointer _ -> CCall (name_exp "TO_FUNC", [exp])
      | _ -> failwith ("bad type to unpack to: " ^
                       Cprint.sprint Cprint.print_ctype ty)
  in

  let box ty exp =
    match ty with
      | CValue -> exp
      | CUInt -> CCall (name_exp "BOX_UINT", [exp])
      | CInt -> CCall (name_exp "BOX_INT", [exp])
      | CFloat -> CCall (name_exp "BOX_FLOAT", [exp])
      | CStr -> CCall (name_exp "BOX_STR", [exp])
      | CClosure _ -> CCall (name_exp "BOX_CLOSURE", [exp])
      | _ -> failwith ("bad type to box: " ^
                       Cprint.sprint Cprint.print_ctype ty)
  in

  let unbox ty exp =
    match ty with
      | CValue -> exp
      | CUInt -> CCall (name_exp "UNBOX_UINT", [exp])
      | CInt -> CCall (name_exp "UNBOX_INT", [exp])
      | CFloat -> CCall (name_exp "UNBOX_FLOAT", [exp])
      | CStr -> CCall (name_exp "UNBOX_STR", [exp])
      | CClosure _ -> CCall (name_exp "UNBOX_CLOSURE", [exp])
      | _ -> failwith ("bad type to unbox: " ^
                       Cprint.sprint Cprint.print_ctype ty)
  in

  let gen_func_ty = CFuncPointer (CPointer CVoid, []) in

  (* Cast helper to deal with the different cases under which casting can occur *)
  let rec cast old_ty new_ty exp =
    if old_ty = new_ty then exp
    else match old_ty, new_ty with
      | CClosure (old_rt, old_args), CClosure (new_rt, new_args) ->
          let cvar = decl_assign ~name:"closure_cast_temp"
            exp (CClosure (old_rt, old_args)) in
          CIdent (downcast_closure (old_rt, old_args) (new_rt, new_args) cvar)
      | _ ->
    if old_ty = CAnyType then
      begin
        match new_ty with
          | CClosure (_, args) ->
              cast
                (CClosure (CValue, repeat CValue (List.length args)))
                new_ty
                (unpack new_ty exp)
          | _ -> unpack new_ty exp
      end
    else if new_ty = CAnyType then
      begin
        match old_ty with
          | CClosure (_, args) ->
              let exp = cast
                old_ty
                (CClosure (CValue, repeat CValue (List.length args)))
                exp
              in
              pack old_ty exp
          | _ -> pack old_ty exp
      end
    else if old_ty = CValue then
      begin
        match new_ty with
          | CClosure (_, args) ->
              cast
                (CClosure (CValue, repeat CValue (List.length args)))
                new_ty
                (unbox new_ty exp)
          | _ -> unbox new_ty exp
      end
    else if new_ty = CValue then
      begin
        match old_ty with
          | CClosure (_, args) ->
              let exp = cast
                old_ty
                (CClosure (CValue, repeat CValue (List.length args)))
                exp
              in
              box old_ty exp
          | _ -> box old_ty exp
      end
    else
      begin
        (* todo: currently just a dirty cast *)
        Printf.eprintf "Warning: dirty cast between %s and %s\n"
          (Cprint.sprint Cprint.print_ctype old_ty)
          (Cprint.sprint Cprint.print_ctype new_ty);
        CCast (exp, new_ty)
      end

  and func_to_var func_var =
    let exp = CCast (CRef (CIdent func_var), gen_func_ty) in
    decl_assign ~name:"func_ptr_temp" exp gen_func_ty

  and assign (target_var, target_ty) (old_exp, old_ty) =
    add @@ CAssign (target_var, cast old_ty target_ty old_exp)

  and make_unit () =
    let var = decl_assign ~name:"unit_var" (CLInt 0) CInt in
    let rvar = fresh_var "unit" in
    set_type rvar CValue;
    add @@ CDecl (rvar, CValue);
    assign (CIdent rvar, CValue) (CIdent var, CInt);
    rvar

  and unify_types t1 t2 =
    if t1 = t2 then t1
    else if t1 = CAnyType then t2
    else if t2 = CAnyType then t1
    else if t1 = CValue then CValue
    else if t2 = CValue then CValue
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

  (* Block helpers *)
  and block_field var n =
    COffset (CArrow (CCall (name_exp "UNBOX_BLOCK", [cast (get_type var) CValue (CIdent var)]), "data"), CLInt n)

  and block_tag exp =
    CArrow (CCall (name_exp "UNBOX_BLOCK", [exp]), "tag")

  and block_size exp =
    CArrow (CCall (name_exp "UNBOX_BLOCK", [exp]), "size")

  (* push args onto a closure, making a new closure
   *)
  and push_closure closure_var func_ptr args new_ty =
    let malloc = closure_malloc (List.length args) in
    let new_closure = decl_assign ~name:"new_closure" malloc new_ty in
    add @@ CAssign (CArrow (CIdent new_closure, "f"), CIdent func_ptr);
    add @@ CAssign (CArrow (CIdent new_closure, "next"), CIdent closure_var);
    List.iteri (fun i x ->
      let var = COffset (CArrow (CIdent new_closure, "args"), CLInt i) in
      add @@ CAssign (var, pack (get_type x) (CIdent x));
    ) args;
    new_closure

  (* pop from a closure, making a new closure object *)
  and pop_closure closure_var new_ty =
    decl_assign ~name:"new_closure"
      (CArrow (CIdent closure_var, "next")) new_ty

  (* Promotes a function to a closure, with the same number of arguments. *)
  and promote func =
    match get_type func with
      (* Temporarily match on both CFuncPointer and CClosure until I can figure
       * out how to separate them
       *)
      | CFuncPointer (rt, arg_tys)
      | CClosure (rt, arg_tys) ->
          let new_arg_tys = arg_tys @ [CClosure (CValue, arg_tys)] in

          let args = enumerate (List.length arg_tys + 1)
            (fun _ -> fresh_var "closure_arg") in
          let arg_with_tys = zip args new_arg_tys in

          let var = fresh_var "closure_promote" in
          let loc = None in

          let (_, body) = with_context (fun () ->
            let call = CCall (CIdent func,
              List.map (fun x -> CIdent x) (init args)) in
            let var = decl_assign ~name:"rvar" call rt in
            add @@ CReturn var;
            var
          ) in

          funcs := {
            return_type = rt;
            args = arg_with_tys;
            id = var;
            body;
            loc;
          } :: !funcs;
          set_type var (CFuncPointer (rt, new_arg_tys));

          let malloc = closure_malloc 0 in
          let closure = decl_assign ~name:"promoted_closure"
            malloc (CClosure (rt, arg_tys)) in
          add @@ CAssign (CArrow (CIdent closure, "f"), CIdent (func_to_var var));
          add @@ CAssign (CArrow (CIdent closure, "next"), CLInt 0);
          closure

      | _ -> failwith "promote called on non-function or closure"


  (* generate a function which represents a function with n arguments pre-applied
   *)
  and closure_n old_arg_tys (rt, arg_tys) =
    (* FIXME: fvar is untyped! This might still be okay as long as we don't
     * try to get the type of fvar! *)
    let n = List.length old_arg_tys in
    let fvar = fresh_var (Printf.sprintf "closure_%d" n) in
    let closure_obj = fresh_var "closure_obj" in
    let args = enumerate (List.length arg_tys) (fun _ ->
      fresh_var "closure_arg") in
    let (_, body) = with_context (fun () ->
      (* make the new closure object, which is the current object but with
       * n + 1 popped off the top
       *)
        let resulting_closure = pop_closure closure_obj
          (CClosure (rt, [])) in

        (* Construct the call. The first n arguments need to
         * come from the closure object
         *)
        let func_ptr = CCast (CArrow (CIdent resulting_closure, "f"),
          CFuncPointer (rt, old_arg_tys @ arg_tys @ [CClosure (rt, [])])) in
        let call_args =
          List.mapi (fun i t ->
            unpack t (closure_data closure_obj i)
          ) old_arg_tys
          @ List.map (fun x -> CIdent x) args
          @ [CIdent resulting_closure] in
        let call = CCall (func_ptr, call_args) in
        let rvar = decl_assign ~name:"closure_return" call rt in
        add @@ CReturn rvar;
        rvar
    ) in
    funcs := {
      return_type = rt;
      args = zip args arg_tys @ [closure_obj, CClosure (CValue, [])];
      id = fvar;
      body;
      loc = None;
    } :: !funcs;
    fvar

  (* Cast a closure down from one taking more arguments to one taking less,
   * and returning another closure
   * e.g. 'a -> 'b -> 'c to 'a -> ('b -> 'c)
   *)
  and downcast_closure (old_rt, old_args) (new_rt, new_args) closure_var =
    let num_args = List.length new_args in
    let (outer_args, inner_args) = split num_args old_args in
    if inner_args = [] then
      (* construct an intermediate closure to cast arguments to the right types
       *)
      let new_ty = CClosure (new_rt, new_args) in

      let fvar = fresh_var "closure_cast" in
      let closure_obj = fresh_var "closure_obj" in
      set_type closure_obj (CClosure (old_rt, old_args));
      let args = enumerate (List.length outer_args) (fun _ ->
        fresh_var "closure_arg") in
      let (_, body) = with_context (fun () ->
        let resulting_args = List.map (fun ((x, t), t') ->
          decl_assign ~name:"arg_cast" (cast t t' (CIdent x)) t')
          (zip (zip args new_args) old_args) in
        let new_closure = pop_closure closure_obj (CClosure (old_rt, old_args)) in
        let rvar = apply_closure new_closure resulting_args in
        let rvar' = cast (get_type rvar) new_rt (CIdent rvar) in
        let rvar'' = decl_assign ~name:"cast_return" rvar' new_rt in
        add @@ CReturn rvar'';
        rvar''
      ) in
      funcs := {
        return_type = new_rt;
        args = zip args new_args @ [closure_obj, get_type closure_obj];
        id = fvar;
        body;
        loc = None;
      } :: !funcs;

      push_closure closure_var (func_to_var fvar) [] new_ty

    else
      (* Construct two functions, one to be the inner function 'b -> 'c
       * the other to be the outer function 'a -> ('b -> 'c)
       *)
      let return_ty = CClosure (old_rt, inner_args) in
      let closure_ty = CClosure (new_rt, new_args) in

      (* Construct inner function *)
      let inner_func = closure_n outer_args (old_rt, inner_args) in

      (* Construct outer function *)
      let outer_func = fresh_var "outer_closure" in
      let outer_closure_obj = fresh_var "closure_obj" in
      let outer_func_arg_tys = new_args @ [closure_ty] in
      let outer_func_args =
        enumerate (List.length outer_args) (fun _ ->
          fresh_var "closure_arg") @ [outer_closure_obj] in
      let outer_func_arg_with_tys = zip outer_func_args outer_func_arg_tys in
      let (_, outer_body) = with_context (fun () ->
        (* the role of the outer function is extremely simple, it just needs
         * to set up the closure stack correctly, and then return it
         *)
        (* Pop by 1 to get rid of the current function pointer off the top of
         * the stack
         *)
        let resulting_closure = pop_closure outer_closure_obj
          (CClosure (old_rt, old_args)) in
        (* Cast the current args into the correct types *)
        let curr_args = init outer_func_arg_with_tys in
        let curr_args = zip curr_args outer_args in
        let closure_args = List.map (fun ((v, old_ty), new_ty) ->
          let exp = cast old_ty new_ty (CIdent v) in
          decl_assign ~name:"call_arg" (pack new_ty exp) CAnyType
        ) curr_args in
        (* Push all the args onto the stack
         *)
        let resulting_closure = push_closure resulting_closure
          (func_to_var inner_func) closure_args return_ty in

        let rvar = decl_assign ~name:"return_cast"
          (cast return_ty new_rt (CIdent resulting_closure)) new_rt in

        add @@ CReturn rvar;
        rvar
      ) in
      funcs := {
        return_type = new_rt;
        args = outer_func_arg_with_tys;
        id = outer_func;
        body = outer_body;
        loc = None
      } :: !funcs;

      (* push just the outer function onto the closure *)
      push_closure closure_var (func_to_var outer_func) []
        (CClosure (new_rt, new_args))

  (* Applies a closure to given args, assuming args have been casted to the
   * right type.
   * Determines if there is partial application and creates a new closure if so.
   *)
  and apply_closure closure_var args =
    match get_type closure_var with
      | CClosure (rt, arg_tys) ->
          if List.length args < List.length arg_tys then
            (* partial application, make a closure! *)
            let n = List.length args in
            let (_, new_arg_tys) = split n arg_tys in
            let new_func = func_to_var (closure_n (List.map get_type args) (rt, new_arg_tys))
            in
            (* push args onto closure object *)
            let resulting_closure = push_closure closure_var new_func args
              (CClosure (rt, new_arg_tys)) in
            resulting_closure
          else
            (* full application *)
            let func_ptr = CCast (CArrow (CIdent closure_var, "f"),
              CFuncPointer (rt, arg_tys @ [CClosure (rt, [])])) in
            let call = CCall (func_ptr, List.map (fun x -> CIdent x) (args @ [closure_var])) in
            let result = decl_assign ~name:"apply_result"
              call rt in
            result


      | _ -> failwith "apply_closure called on non-closure"


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
    | Const_pointer n -> (CCall (name_exp "BOX_INT", [CLInt n]), CValue)

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
              | CClosure (rt, arg_tys) ->
                  (* TODO: dedup this and comp_expr? *)
                  let loc = unpack_loc !curr_loc in
                  let params = List.map (fun x -> CVar x) params in

                  (* check if the function requires eta expansion
                   * this occurs when the type of the function takes more
                   * arguments than actual parameters
                   *)
                  let _, expand_tys = split (List.length params) arg_tys in
                  (* generate new params *)
                  let new_params = enumerate (List.length expand_tys) (fun _ ->
                    fresh_var "eta_expand_var") in
                  let params = params @ new_params in

                  let args = zip params arg_tys in

                  (* set types for each of the args, just in case *)
                  List.iter (fun (arg, ty) -> set_type arg ty) args;
                  (* Drop into non-toplevel for function body *)
                  let (rvar, fblock) = with_context (fun () ->
                    let rvar = comp_expr body in

                    (* if eta expansion happened, make sure to actually apply the
                     * remaining args
                     *)
                    let rvar' = if new_params <> [] then
                      apply_closure rvar new_params
                    else
                      rvar
                    in

                    let rvar'' = decl_assign ~name:"return_value"
                      (cast (get_type rvar') rt (CIdent rvar')) rt in

                    add (CReturn rvar'');
                    rvar''
                  ) in
                  let temp_var = fresh_var "func" in
                  set_type temp_var (CFuncPointer (rt, arg_tys));
                  funcs := {
                    return_type = rt;
                    args;
                    id = temp_var;
                    body = fblock;
                    loc = Some loc;
                  } :: !funcs;
                  let closure = promote temp_var in
                  preamble := CDecl (CVar id, get_type (CVar id)) :: !preamble;
                  add (CAssign (CIdent (CVar id), (CIdent closure)))

              | _ -> failwith "type of function was not a closure"
          end;
          comp_root rest

      | Llet (_, _, id, arg, rest) ->
          (* Declare a global constant *)
          let ty = get_type (CVar id) in
          preamble := CDecl (CVar id, ty) :: !preamble;
          let res = comp_expr ~type_hint:(Some ty) arg in
          assign (CIdent (CVar id), ty) (CIdent res, get_type res);
          comp_root rest

      | Lletrec (decls, body) ->
          comp_letrec ~at_root:true decls;
          comp_root body
          (*
          (* Dirty hack - does this always work? *)
          let expr = List.fold_right (fun (id, arg) rest ->
            Llet(Strict, Pgenval, id, arg, rest)) decls body in
          comp_root expr
          *)

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
  and comp_expr ?(type_hint=None) ?(use_name=None) exp =
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

          begin
            match get_type fvar with
              | CFuncPointer (rt, arg_tys) ->
                  (*
                  if List.length arg_tys <> List.length args
                  then failwith "Partial application is not supported"
                  else

                    let exp = CCall
                      (CIdent fvar, List.map (fun x -> CIdent x) args) in
                    decl_assign ~name:"call_result" exp rt
                  *)
                  failwith "Bare function pointer call!"

              | CPointer CVoid ->
                  (*
                  Printf.eprintf "Warning: void* call\n";
                  let exp = CCall
                    (CIdent fvar, List.map (fun x -> CIdent x) args) in
                  decl_assign ~name:"call_result" exp (CPointer CVoid)
                  *)
                  failwith "Bare void* call!"

              | CClosure (rt, arg_tys) ->
                  (* cast args to the correct types *)
                  let new_args = List.map (fun (x, t) ->
                    let exp = cast (get_type x) t (CIdent x) in
                    decl_assign ~name:"apply_cast" exp t) (zip args arg_tys) in
                  apply_closure fvar new_args

              (* TODO: deal with cvalues as function pointers *)

              | CValue
              | CAnyType ->
                  (* gotta assume full application *)
                  (* TODO: yeah this is a massive hole for the type system. *)
                  let target_type = CClosure (CValue, repeat CValue (List.length args)) in
                  let fexp = cast (get_type fvar) target_type (CIdent fvar) in
                  let fvar = decl_assign ~name:"func_cast" fexp target_type in
                  let new_args = List.map (fun x ->
                    let exp = cast (get_type x) CValue (CIdent x) in
                    decl_assign ~name:"apply_cast" exp CValue) args in
                  apply_closure fvar new_args

              | _ -> failwith
                ((Cprint.sprint Cprint.print_cident fvar) ^ " was not a function pointer")
          end

      | Lfunction { params; body } ->
          let (rt, arg_tys) = match type_hint with
            | Some (CClosure (rt, arg_tys)) ->
              (* set types for each of the args, just in case *)
              let args = zip (List.map (fun x -> CVar x) params) arg_tys in
              List.iter (fun (arg, ty) -> set_type arg ty) args;
              (rt, arg_tys)
            | _ ->
                Printf.eprintf "Warning: no type hints for function argument, trying to use found types\n";
                (CValue, List.map (fun id ->
                  try
                    IdentHash.find types id
                  with Not_found ->
                    set_type (CVar id) CValue;
                    CValue
                  ) params)
          in

          let params = List.map (fun x -> CVar x) params in

          (* check if the function requires eta expansion
           * this occurs when the type of the function takes more
           * arguments than actual parameters
           *)
          let _, expand_tys = split (List.length params) arg_tys in
          (* generate new params *)
          let new_params = enumerate (List.length expand_tys) (fun _ ->
            fresh_var "eta_expand_var") in
          let params = params @ new_params in

          let args = zip params arg_tys in

          (* find free variables *)
          let fvs = List.map (fun x -> CVar x)
            (IdentSet.elements @@ free_variables exp) in
          let n_fvs = List.length fvs in

          let closure_arg = fresh_var "closure_obj" in
          let closure_ty = CClosure (rt, arg_tys) in

          let function_name = fresh_var "local_func" in
          let loc = unpack_loc !curr_loc in

          let (_, body) = with_context (fun () ->
            (* get the free variables from the closure *)
            List.iteri (fun i fv ->
              let ty = get_type fv in
              add @@ CDecl (fv, ty);
              add @@ CAssign (CIdent fv, unpack ty (closure_data closure_arg i))
            ) fvs;

            let rvar = comp_expr body in
            (* if eta expansion happened, make sure to actually apply the
             * remaining args
             *)
            let rvar' = if new_params <> [] then
              apply_closure rvar new_params
            else
              rvar
            in

            let rvar'' = decl_assign ~name:"return_value"
              (cast (get_type rvar') rt (CIdent rvar')) rt in

            add (CReturn rvar'');
            rvar''
          ) in

          (* add the new function *)
          funcs := {
            return_type = rt;
            args = args @ [closure_arg, closure_ty];
            id = function_name;
            body;
            loc = Some loc;
          } :: !funcs;

          let func_var = func_to_var function_name in

          let resulting_closure = match use_name with
            | Some var -> var
            | None ->
              let malloc = closure_malloc n_fvs in
              decl_assign ~name:"closure_obj" malloc closure_ty
          in
          add @@ CAssign (CArrow (CIdent resulting_closure, "f"), CIdent func_var);
          (* add all the free vars to it *)
          List.iteri (fun i v ->
            add @@ CAssign (closure_data resulting_closure i, pack (get_type v) (CIdent v))
          ) fvs;

          resulting_closure

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
            let rvar = comp_expr ~type_hint:(Some arg_ty) body in
            assign (CIdent temp, get_type rvar) (CIdent rvar, get_type rvar);
            rvar
          ) in
          let rty = get_type rvar in
          set_type temp rty;
          add (CDecl (temp, rty));
          add (CBlock (lblock));
          temp

      | Lletrec (decls, body) ->
          comp_letrec decls;
          comp_expr body
          (*
          (* FIXME: fix letrec behavior *)
          let expr = List.fold_right (fun (id, arg) rest ->
            Llet(Strict, Pgenval, id, arg, rest)) decls body in
          comp_expr expr
           *)

      | Lprim (p, ls, loc) ->
          curr_loc := loc;
          comp_prim ~use_name p ls

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
          let rty = List.fold_left unify_types CAnyType
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

          let (_, int_switch) = with_context (fun () ->
            add @@ CSwitch
            (CCall (name_exp "UNBOX_INT", [var]), int_blocks, fail_goto);
            fresh_var "dummy_var"
          ) in
          let (_, tag_switch) = with_context (fun () ->
            add @@ CSwitch
            (block_tag var, tag_blocks, fail_goto);
            fresh_var "dummy_var"
          ) in

          (* Construct the if statement *)
          let ifstmt = CIfElse (condition, int_switch,
            match fail_block with
              | Some b -> [CIfElse (CLInt 1, tag_switch, b)]
              | None -> tag_switch
            ) in

          add (CDecl (rvar, rty));
          set_type rvar rty;
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
          (* HMMMMM *)
          let iexp = cast (get_type ivar) CValue (CIdent ivar) in
          let iexp = cast CValue CInt (iexp) in
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
          set_type cvar CValue;
          add (CDecl (cvar, CValue));

          let ctemp = comp_expr cond in
          assign (CIdent cvar, CValue) (CIdent ctemp, get_type ctemp);

          let (_, block) = with_context (fun () ->
            comp_expr body |> ignore;
            let ctemp = comp_expr cond in
            assign (CIdent cvar, CValue) (CIdent ctemp, get_type ctemp);
            ctemp
          ) in
          add (CWhile (cast CValue CInt (CIdent cvar), block));
          make_unit ()

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

          let (_, block) = with_context (fun () ->
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

            make_unit ()
          ) in
          add @@ CBlock block;
          (* for loops return unit *)
          make_unit ()

      | Lassign _ -> failwith "undefined"

      | Lstaticcatch (body, (id, []), catch) ->
          let (rvar, rblock) = with_context (fun () -> comp_expr body) in
          let (cvar, cblock) = with_context (fun () ->
            add (CLabel (Printf.sprintf "staticcatch_%d" id));
            comp_expr catch) in
          let var = fresh_var "catch_return" in
          let ty = unify_types (get_type rvar) (get_type cvar) in
          add (CDecl (var, ty));
          set_type var ty;
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
          decl_assign ~name:"dead_var" (cast CInt CValue (CLInt 1)) CValue

      | Levent (lam, ev) ->
          set_loc ev;
          comp_expr lam

      | _ -> failwith "Unsupported lambda term"

  (* Compiles a series of letrec declarations. *)
  and comp_letrec ?(at_root=false) decls =
    (* compilation strategy: for each lambda term,
     * if it contains no free occurrence of any of the new names, compile it
     * straight away
     * otherwise, work out the size of the resulting expression, make the
     * declaration and malloc, and defer compilation to after all the
     * declarations for the other variables are known.
     *)
    let ids = IdentSet.of_list @@ List.map (fun (x, _) -> x) decls in

    (* special decl to switch on at_root to see if a declaration should be
     * added to the preamble or not
     *)
    let decl var ty =
      if at_root then
        preamble := CDecl (var, ty) :: !preamble
      else
        add @@ CDecl (var, ty)
    in

    let to_comp = ref [] in

    let rec phase_one (id, lam) =
      let var = CVar id in
      let fvs = free_variables lam in
      if IdentSet.is_empty @@ IdentSet.inter ids fvs then
        (* No free occurrence of new names, compile straight away *)
        let rvar = (if at_root then comp_root else comp_expr ~type_hint:(Some (get_type var)) ~use_name:None) lam in
        decl var (get_type var);
        assign (CIdent var, get_type var)
               (CIdent rvar, get_type rvar)
      else begin
        match lam with
          | Levent (body, ev) ->
              set_loc ev;
              phase_one (id, body)
          | Lfunction { params; body } ->
              let n_fvs = List.length @@ IdentSet.elements fvs in
              let malloc = closure_malloc n_fvs in
              let ty = match get_type var with
                | CFuncPointer (rt, arg_tys)
                | CClosure (rt, arg_tys) ->
                    CClosure (rt, arg_tys)
                | _ -> failwith "Lfunction doesn't have valid type!"
              in
              decl var ty;
              (* no need for casting *)
              add @@ CAssign (CIdent var, malloc);
              to_comp := !to_comp @ [var, get_type var, lam]
          | Lprim (Pmakeblock _, contents, _) ->
              (* size of block is number of elements in body +1 for the tag *)
              let size = List.length contents in
              let sizeof = CBinOp ("+", CSizeOf CBlockT,
                CBinOp ("*", CSizeOf CAnyType, CLInt size)) in
              let malloc = CCall (name_exp "MALLOC", [sizeof]) in
              let block = CCall (name_exp "BOX_BLOCK", [malloc]) in
              decl var (get_type var);
              add @@ CAssign (CIdent var, block);
              to_comp := !to_comp @ [var, get_type var, lam]
          | _ -> failwith "Invalid RHS encountered in letrec compilation"
      end
    in

    List.iter phase_one decls;

    let rec phase_two (var, ty, lam) =
      comp_expr ~type_hint:(Some ty) ~use_name:(Some var) lam |> ignore
    in

    List.iter phase_two !to_comp


  (* Compiles a Lprimitive expression. *)
  and comp_prim ?(use_name=None) prim lambdas =
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

    let int_to_bool var =
      decl_assign ~name:"bool_cast" (cast CInt CValue (CIdent var)) CValue
    in
    let bool_bop op e1 e2 = int_to_bool (bop CInt CInt op e1 e2) in
    let int_unop = unop CInt CInt in
    let int_bop = bop CInt CInt in
    let int_cmp_bop op e1 e2 = int_to_bool (bop CInt CInt op e1 e2) in
    let float_unop = unop CFloat CFloat in
    let float_bop = bop CFloat CFloat in
    let float_cmp_bop op e1 e2 = int_to_bool (bop CFloat CInt op e1 e2) in

    match prim, lambdas with
      | Pmakeblock (tag, _, _), contents ->
          let vars = List.map comp_expr contents in
          let blocklen = List.length vars in
          let sizeof = CBinOp ("+", CSizeOf CBlockT,
            CBinOp ("*", CSizeOf CAnyType, CLInt blocklen)) in
          let malloc_expr = CCall (name_exp "MALLOC", [sizeof]) in
          let block_expr = CCall (name_exp "BOX_BLOCK", [malloc_expr]) in
          let block = match use_name with
            | Some var -> var
            | None -> decl_assign ~name:"block" block_expr CValue in
          assign (block_tag (CIdent block), CInt) (CLInt tag, CInt);
          assign (block_size (CIdent block), CInt) (CLInt blocklen, CInt);
          List.iteri (fun n v ->
            assign (block_field block n, CAnyType)
                   (* when assigning to a block, go through CValue first *)
                   (cast (get_type v) CValue (CIdent v), CValue)
          ) vars;
          block

      | Pidentity, [x] -> comp_expr x

      (* Ignore result and return NULL *)
      | Pignore, [x] ->
          comp_expr x |> ignore;
          make_unit ()

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
          (* reading from a block should be a CValue *)
          let e = comp_expr lam in
          let var = fresh_var "field_access" in
          let ty = CValue in
          set_type var ty;
          add (CDecl (var, ty));
          assign (CIdent var, ty) (block_field e i, CAnyType);
          var

      | Psetfield (i, _, _), [trg; lam]
      | Psetfloatfield (i, _), [trg; lam] ->
          (* writing to a block should go through CValue *)
          let block = comp_expr trg in
          let var = comp_expr lam in
          assign (block_field block i, CAnyType)
                 (cast (get_type var) CValue (CIdent var), CValue);
          make_unit ()

      | Psequand, [e1; e2] -> bool_bop "&&" e1 e2
      | Psequor, [e1; e2] -> bool_bop "||" e1 e2
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
      | Pintcomp (Ceq), [e1; e2] -> int_cmp_bop "==" e1 e2
      | Pintcomp (Cneq), [e1; e2] -> int_cmp_bop "!=" e1 e2
      | Pintcomp (Clt), [e1; e2] -> int_cmp_bop "<" e1 e2
      | Pintcomp (Cle), [e1; e2] -> int_cmp_bop "<=" e1 e2
      | Pintcomp (Cgt), [e1; e2] -> int_cmp_bop ">" e1 e2
      | Pintcomp (Cge), [e1; e2] -> int_cmp_bop ">=" e1 e2
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
          decl_assign ~name:"isint" (CCall (name_exp "IS_INT", [CIdent var])) CInt

      | Pccall { prim_name; prim_arity }, args ->
          begin
            assert (List.length args = prim_arity);
            try
              let func_type = ExternHash.find externals prim_name in
              match func_type with
                | CFuncPointer (rty, arg_tys)
                | CClosure (rty, arg_tys) ->
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

