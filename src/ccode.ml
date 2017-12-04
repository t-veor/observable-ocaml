(* ccode.ml *)
(* ML representation of a subset of C to compile to. *)

type cvar = Ident.t

and cexpr = 
  | CVar of cvar
  | CLInt of int
  | CLChar of char
  | CLString of string
  | CLFloat of float
  | CUnOp of cunop * cexpr
  | CBinOp of cbinop * cexpr * cexpr
  | CCall of cvar * cexpr list
  | CCast of cexpr * ctype

and cstatement = 
  | CDecl of cvar * ctype
  | CAssign of cvar * cexpr
  | CReturn of cvar
  | CBlock of cblock
  | CIfElse of cexpr * cblock * cblock
  | CWhile of cexpr * cblock
  | CLoc of int * string

and cblock = cstatement list

and cfunc = {
  return_type : ctype;
  args : (cvar * ctype) list;
  id : cvar;
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
  | CVoid
  | CPointer of ctype
  | CFuncPointer of ctype * ctype list

and ccode = {
  preamble : cstatement list;
  funcs : cfunc list;
  main : cblock;
}
