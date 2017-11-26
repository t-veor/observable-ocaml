open Ccode
open Printf

exception NotSupported of string

let rec comma_sep print_f out = function
  | [] -> ()
  | [x] -> print_f out x
  | x::xs -> print_f out x; comma_sep print_f out xs

let rec print_cvar out = function
  | Var (s, _) -> fprintf out "%s" s
  | TempVar i -> fprintf out "__tempvar_%d" i

and print_cexpr out = function
  | CVar v -> print_cvar out v
  | CLInt i -> fprintf out "%d" i
  | CLChar c -> fprintf out "%C" c
  | CLString s -> fprintf out "%S" s
  | CLFloat f -> fprintf out "%F" f
  | CUnOp (op, v) ->
      fprintf out "(";
      print_cunop out op;
      print_cvar out v;
      fprintf out ")"
  | CBinOp (op, v1, v2) ->
      fprintf out "(";
      print_cvar out v1;
      print_cbinop out op;
      print_cvar out v2;
      fprintf out ")"
  | CCall (v, vs) ->
      print_cvar out v;
      fprintf out "(";
      comma_sep print_cvar out vs;
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

and print_cblock out block =
  fprintf out "{";
  List.fold_left (fun () x -> print_cstatement out x) () block |> ignore;
  fprintf out "}"

and print_cfunc out = function
  | { return_type; args; id; body } ->
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

and print_cunop out = function
  | CNot -> fprintf out "!"
  | CNeg -> fprintf out "-"

and print_cbinop out = function
  | CAdd -> fprintf out "+"
  | CSub -> fprintf out "-"
  | CMul -> fprintf out "*"
  | CDiv -> fprintf out "/"
  | CAnd -> fprintf out "&&"
  | COr -> fprintf out "||"

and print_ctype out = function
  | CInt -> fprintf out "int"
  | CFloat -> fprintf out "double"
  | CStr -> fprintf out "char*"
  | CVoid -> fprintf out "void"
  | CPointer t ->
      print_ctype out t;
      fprintf out "*"
  (* TODO: probably unify printing types and variables *)
  | CFuncPointer _ -> raise (NotSupported "Function pointers are not supported")

and print_ccontext out = function
  | { funcs; main } ->
      List.fold_left (fun () f ->
        print_cfunc out f;
        fprintf out "\n") () funcs;
      fprintf out "int main()";
      print_cblock out main

