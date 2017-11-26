(* typecollect.ml *)
(* Collects all types of variables from a Typedtree.structure. *)

open Typedtree
open TypedtreeIter

module TypeCollectorArgument = struct
  include DefaultIteratorArgument

  let type_list = ref ([] : (Ident.t * Types.type_expr) list)

  let enter_pattern p = match p.pat_desc with
    | Tpat_var (id, _) ->
      type_list := (id, p.pat_type) :: !type_list
    | Tpat_alias (_, id, _) ->
      type_list := (id, p.pat_type) :: !type_list
    | _ -> ()

  let reset () = type_list := []
end

module TypeCollector = TypedtreeIter.MakeIterator(TypeCollectorArgument)

let types typedtree =
  TypeCollectorArgument.reset ();
  TypeCollector.iter_structure typedtree;
  !TypeCollectorArgument.type_list
