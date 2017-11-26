(* ccode.ml *)
(* ML representation of a subset of C to compile to. *)

type cvar =
  | Var of string * int
  | TempVar of int

and cexpr = 
  | CVar of cvar
  | CLInt of int
  | CLChar of char
  | CLString of string
  | CLFloat of float
  | CUnOp of cunop * cvar
  | CBinOp of cbinop * cvar * cvar
  | CCall of cvar * cvar list

and cstatement = 
  | CDecl of cvar * ctype
  | CAssign of cvar * cexpr
  | CReturn of cvar
  | CBlock of cblock
  | CIfElse of cexpr * cblock * cblock
  | CWhile of cexpr * cblock

and cblock = cstatement list

and cfunc = {
  return_type : ctype;
  args : (cvar * ctype) list;
  id : cvar;
  body : cblock;
}

and cunop =
  | CNot
  | CNeg

and cbinop =
  | CAdd
  | CSub
  | CMul
  | CDiv
  | CAnd
  | COr

and ctype =
  | CInt
  | CFloat
  | CStr (* placeholder before we get proper strings *)
  | CVoid
  | CPointer of ctype
  | CFuncPointer of ctype * ctype list

and ccontext = {
  funcs : cfunc list;
  main : cblock;
}

let append ctx stmts =
  let { funcs; main } = ctx in
  { funcs = funcs; main = main @ stmts }

let lift ctx return_type args id =
  let { funcs; main } = ctx in
  {
    funcs = { return_type; args; id; body = main } :: funcs;
    main = [];
  }

let blank_ctx = { funcs = []; main = [] }
