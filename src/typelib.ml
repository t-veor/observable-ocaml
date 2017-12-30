(* typelib.ml *)
(* Compiles OCaml types into C types. *)

open Types
open Ccode

(* Ignore level for now *)
let rec comp_type = function
  { desc } -> match desc with
    | Tvar _ -> failwith "Type variables are not supported"

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

    | Tconstr _ -> CValue

    | Tlink t -> comp_type t

    | _ -> failwith "Unsupported type"
