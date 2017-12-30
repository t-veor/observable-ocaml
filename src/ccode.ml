(* ccode.ml *)
(* ML representation of a subset of C to compile to. *)

type cident =
  | CVar of Ident.t
  | CGlobalVar of string

and cexpr =
  | CIdent of cident
  | CRef of cexpr
  | CDeref of cexpr
  | CField of cexpr * string
  | COffset of cexpr * int
  | CLInt of int
  | CLChar of char
  | CLString of string
  | CLFloat of float
  | CUnOp of cunop * cexpr
  | CBinOp of cbinop * cexpr * cexpr
  | CCall of cexpr * cexpr list
  | CCast of cexpr * ctype
  | CSizeOf of ctype

and cstatement =
  | CDecl of cident * ctype
  | CAssign of cexpr * cexpr
  | CReturn of cident
  | CBlock of cblock
  | CIfElse of cexpr * cblock * cblock
  | CWhile of cexpr * cblock
  | CLoc of int * string
  | CInclude of string

and cblock = cstatement list

and cfunc = {
  return_type : ctype;
  args : (cident * ctype) list;
  id : cident;
  body : cblock;
  loc : int * string;
}

and cunop = string

and cbinop = string

and ctype =
  | CUInt
  | CInt
  | CFloat
  | CStr (* placeholder before we get proper strings *)
  | CValue (* generic ocaml_t type *)
  | CBlockT (* ocaml_block_t type, for Pmakeblock *)
  | CVoid
  | CPointer of ctype
  | CNamedType of Ident.t
  | CFuncPointer of ctype * ctype list

and ccode = {
  preamble : cstatement list;
  funcs : cfunc list;
  main : cblock;
}
