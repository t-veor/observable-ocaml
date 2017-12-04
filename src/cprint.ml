open Ccode
open Format

exception NotSupported of string

let rec comma_sep print_f out = function
  | [] -> ()
  | [x] -> print_f out x
  | x::xs ->
      print_f out x;
      fprintf out ",";
      comma_sep print_f out xs

let rec print_cvar out var =
  fprintf out "%s" (Ident.unique_name var)

and print_cexpr out = function
  | CVar v -> print_cvar out v
  | CLInt i -> fprintf out "%d" i
  | CLChar c -> fprintf out "%C" c
  | CLString s -> fprintf out "%S" s
  | CLFloat f -> fprintf out "%F" f
  | CUnOp (op, e) ->
      fprintf out "(";
      fprintf out "%s" op;
      print_cexpr out e;
      fprintf out ")"
  | CBinOp (op, e1, e2) ->
      fprintf out "(";
      print_cexpr out e1;
      fprintf out "%s" op;
      print_cexpr out e2;
      fprintf out ")"
  | CCall (v, es) ->
      print_cvar out v;
      fprintf out "(";
      comma_sep print_cexpr out es;
      fprintf out ")"
  | CCast (e, t) ->
      fprintf out "((";
      print_ctype out t;
      fprintf out ")";
      print_cexpr out e;
      fprintf out ")"


and print_cstatement out = function
  | CDecl (v, t) ->
      print_ctype out t;
      fprintf out " ";
      print_cvar out v;
      fprintf out ";"
  | CAssign (v, e) ->
      print_cvar out v;
      fprintf out "=";
      print_cexpr out e;
      fprintf out ";"
  | CReturn v ->
      fprintf out "return ";
      print_cvar out v;
      fprintf out ";"
  | CBlock block ->
      fprintf out "{";
      print_cblock out block;
      fprintf out "}"
  | CIfElse (e, b1, b2) ->
      fprintf out "if(";
      print_cexpr out e;
      fprintf out ")";
      print_cblock out b1;
      fprintf out "else";
      print_cblock out b2;
  | CWhile (e, b) ->
      fprintf out "while(";
      print_cexpr out e;
      fprintf out ")";
      print_cblock out b;
  | CLoc (l, f) ->
      fprintf out "\n#line %d %s\n" l f

and print_cblock out block =
  fprintf out "{";
  List.fold_left (fun () x -> print_cstatement out x) () block |> ignore;
  fprintf out "}"

and print_cfunc out = function
  | { return_type; args; id; body; loc = (lnum, fname) } ->
      print_cstatement out (CLoc (lnum, fname));
      print_ctype out return_type;
      fprintf out " ";
      print_cvar out id;
      fprintf out "(";
      comma_sep (fun out (v, t) ->
        print_ctype out t;
        fprintf out " ";
        print_cvar out v) out args;
      fprintf out ")";
      print_cblock out body

and print_ctype out = function
  | CUInt -> fprintf out "unsigned int"
  | CInt -> fprintf out "int"
  | CFloat -> fprintf out "double"
  | CStr -> fprintf out "char*"
  | CVoid -> fprintf out "void"
  | CPointer t ->
      print_ctype out t;
      fprintf out "*"
  (* TODO: probably unify printing types and variables *)
  | CFuncPointer (ret, args) ->
      fprintf out "(";
      print_ctype out ret;
      fprintf out ")";
      fprintf out "(*f)";
      fprintf out "(";
      comma_sep print_ctype out args;
      fprintf out ")"
      (* raise (NotSupported "Function pointers are not supported") *)

and print_ccode out = function
  | { preamble; funcs; main } ->
      List.fold_left (fun () s ->
        print_cstatement out s) () preamble;
      List.fold_left (fun () f ->
        print_cfunc out f;
        fprintf out "\n") () funcs;
      fprintf out "int main()";
      print_cblock out main

