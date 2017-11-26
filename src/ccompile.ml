open Lambda
open Types
open Ccode
open Asttypes

exception Undefined
exception NotSupported of string

let undefined () = raise Undefined

let curr_id = ref 0
let fresh_var () =
  let id = !curr_id in
  curr_id := id + 1; TempVar id

module TypeMap = Map.Make(struct
  type t = cvar
  let compare x y = match x, y with
    | (Var (x, y), Var (z, w)) -> Pervasives.compare (x, y) (z, w)
    | (Var _, TempVar _) -> 1
    | (TempVar _, Var _) -> -1
    | (TempVar x, TempVar y) -> Pervasives.compare x y
end)

(* Ignore level for now *)
let rec comp_type = function
  { desc } -> match desc with
    | Tvar _ -> raise (NotSupported "Type variables are not supported")

    | Tarrow (_, t1, t2, _) ->
        let t1 = comp_type t1 in begin
        match comp_type t2 with
          | CFuncPointer (rt, ts) -> CFuncPointer (rt, t1 :: ts)
          | t2 -> CFuncPointer (t2, [t1])
        end

    (* Replace with more robust system later *)

    | Tconstr (Path.Pident { Ident.name = "int" }, [], _) -> CInt
    | Tconstr (Path.Pident { Ident.name = "float" }, [], _) -> CFloat
    | Tconstr (Path.Pident { Ident.name = "bool" }, [], _) -> CInt
    | Tconstr (Path.Pident { Ident.name = "unit" }, [], _) -> CPointer CVoid

    | Tlink t -> comp_type t

    | _ -> raise (NotSupported "Unsupported type")

let cvar_of_ident = function
  { Ident.name = name; Ident.stamp = stamp } -> Var (name, stamp)

let comp_types =
  List.fold_left (fun m (id, ty) ->
    TypeMap.add (cvar_of_ident id) (comp_type ty) m) TypeMap.empty


let comp_constant = function
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

  | _ -> raise (NotSupported "Unsupported constant")

let rec comp_expr ctx exp types =
  match exp with
    | Lvar id ->
        (cvar_of_ident id, ctx, types)

    | Lconst const ->
        let var = fresh_var () in
        let (exp, ty) = comp_constant const in
        ( var
        , append ctx [CDecl (var, ty); CAssign (var, exp)]
        , TypeMap.add var ty types
        )

    | Lapply { ap_func; ap_args } ->
        let (fvar, ctx, types) = comp_expr ctx ap_func types in
        let (args, ctx, types) = List.fold_left
          (fun (args, ctx, types) exp ->
            let (arg, ctx, types) = comp_expr ctx exp types in
            (arg :: args, ctx, types)) ([], ctx, types) ap_args in
        let args = List.rev args in

        (* Assert no partial application *)
        let CFuncPointer (rt, arg_tys) = TypeMap.find fvar types in
        if List.length arg_tys != List.length args
        then raise (NotSupported "Partial application is not supported")
        else

          let exp = CCall (fvar, args) in
          let var = fresh_var () in
          let stmt = [CDecl (var, rt); CAssign (var, exp)] in
          (var, append ctx stmt, types)

    | Lfunction { params; body } ->
        (*
        let (rvar, ctx, _) = comp_expr ctx body types in
        let ctx = append ctx [CReturn rvar] in
        let var = match v with
          | Some v -> v
          | None -> fresh_var ()
        in
        let params = List.map (fun x ->
          (cvar_of_ident x, TypeMap.find (cvar_of_ident x) types)) params in
        let rt = TypeMap.find rvar types in
        let ty = CFuncPointer (rt, (List.map (fun (_, x) -> x) params)) in
        let types = TypeMap.add var ty types in
        let ctx = lift ctx rt params var in
        (var, ctx, types)
        *)
        raise (NotSupported "Bare Lfunction")

    | Llet(_, _, id, Lfunction { params; body }, rest) ->
        let nctx = { funcs = ctx.funcs; main = [] } in
        let (rvar, nctx, _) = comp_expr nctx body types in
        let nctx = append nctx [CReturn rvar] in
        let var = cvar_of_ident id in
        let params = List.map (fun x ->
          (cvar_of_ident x, TypeMap.find (cvar_of_ident x) types)) params in
        let rt = TypeMap.find rvar types in
        let ty = CFuncPointer (rt, (List.map (fun (_, x) -> x) params)) in
        let types = TypeMap.add var ty types in
        let nctx = lift nctx rt params var in

        let ctx = { funcs = nctx.funcs; main = ctx.main } in
        comp_expr ctx rest types
    
    | Llet (_, _, id, arg, body) ->
        let (var, ctx, types) = comp_expr ctx arg types in
        let id = cvar_of_ident id in
        let ty = TypeMap.find id types in
        let nctx = {
          funcs = ctx.funcs;
          main = [CDecl (id, ty); CAssign (id, CVar var)];
        } in
        let (nvar, nctx, types) = comp_expr nctx body types in

        let fvar = fresh_var () in
        let rt = TypeMap.find nvar types in
        let types = TypeMap.add fvar rt types in
        let ctx = {
          funcs = nctx.funcs;
          main = ctx.main @ [CDecl (fvar, rt); CBlock nctx.main];
        }
        in
        (fvar, ctx, types)

    | Lletrec (decls, body) ->
        let expr = List.fold_right (fun (id, arg) rest ->
          Llet(Strict, Pgenval, id, arg, rest)) decls body in
        comp_expr ctx expr types

    | Lprim (p, ls, loc) ->
        comp_prim ctx p ls types

    | Lswitch _ -> undefined ()

    | Lifthenelse _ -> undefined ()

    | Lsequence _ -> undefined ()

    | Lwhile _ -> undefined ()

    | Lfor _ -> undefined ()

    | Lassign _ -> undefined ()

    | _ -> raise (NotSupported "Unsupported Lambda term")

and comp_prim ctx prim lambdas types = match prim with
  (* Just ignore for now! *)
  | Psetglobal _ -> comp_expr ctx (List.hd lambdas) types
  | Pmakeblock _ ->
      let var = fresh_var () in
      let ty = CPointer CVoid in
      let types = TypeMap.add var ty types in
      (var, ctx, types)
  | _ -> raise (NotSupported "Unsupported primitive")

let compile lambda types =
  let types = comp_types types in
  let (_, ctx, _) = comp_expr blank_ctx lambda types in
  ctx
