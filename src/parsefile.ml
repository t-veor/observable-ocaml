(* parsefile.ml *)
(* Uses the OCaml compiler front-end to parse a surface AST. *)

let parse_file sourcefile =
  Location.input_name := sourcefile;
  let ic = open_in_bin sourcefile in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf sourcefile;
  let ast = Parse.implementation lexbuf in
  Ast_invariants.structure ast;
  ast
