(* typelib.ml *)
(* Compiles OCaml types into C types. *)

open Types
open Ccode

(* Ignore level for now *)
let rec comp_type = function
  { desc } -> match desc with
    | Tvar _ -> CValue

    | Tarrow (_, t1, t2, _) ->
        (* Compile everything as CClosure temporarily *)
        let t1 = comp_type t1 in begin
        match comp_type t2 with
          | CFuncPointer (rt, ts)
          | CClosure (rt, ts) ->
              CClosure (rt, t1 :: ts)
          | t2 ->
              CClosure (t2, [t1])
        end

    (* Replace with more robust system later *)

    | Tconstr (Path.Pident { Ident.name = "int" }, [], _) -> CInt
    | Tconstr (Path.Pident { Ident.name = "float" }, [], _) -> CFloat
    | Tconstr (Path.Pident { Ident.name = "bool" }, [], _) -> CValue
    | Tconstr (Path.Pident { Ident.name = "string" }, [], _) -> CStr
    | Tconstr (Path.Pident { Ident.name = "unit" }, [], _) -> CValue

    | Tconstr _ -> CValue

    | Tlink t -> comp_type t

    | Ttuple _ -> CValue

    | _ -> failwith "Unsupported type"
