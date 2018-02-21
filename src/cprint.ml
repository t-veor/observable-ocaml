open Ccode
open Format

exception NotSupported of string

let sprint f e =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  f fmt e;
  Format.pp_flush_formatter fmt;
  Buffer.contents buf


let rec comma_sep print_f out = function
  | [] -> ()
  | [x] -> print_f out x
  | x::xs ->
      print_f out x;
      fprintf out ",";
      comma_sep print_f out xs


let rec print_cident out = function
  | CVar var ->
      fprintf out "%s" (Ident.unique_name var)
  | CGlobalVar s ->
      fprintf out "%s" s


and print_cexpr out = function
  | CIdent i ->
      print_cident out i
  | CRef e ->
      fprintf out "&(";
      print_cexpr out e;
      fprintf out ")"
  | CDeref e ->
      fprintf out "*(";
      print_cexpr out e;
      fprintf out ")"
  | CField (e, f) ->
      fprintf out "(";
      print_cexpr out e;
      fprintf out ").%s" f
  | CArrow (e, f) ->
      fprintf out "(";
      print_cexpr out e;
      fprintf out ")->%s" f
  | COffset (e, i) ->
      print_cexpr out e;
      fprintf out "[";
      print_cexpr out i;
      fprintf out "]"
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
      print_cexpr out v;
      fprintf out "(";
      comma_sep print_cexpr out es;
      fprintf out ")"
  | CCast (e, t) ->
      fprintf out "((";
      print_ctype out t;
      fprintf out ")";
      print_cexpr out e;
      fprintf out ")"
  | CSizeOf t ->
      fprintf out "sizeof(";
      print_ctype out t;
      fprintf out ")"


and print_cstatement out = function
  | CBare e ->
      print_cexpr out e;
      fprintf out ";"
  | CDecl (v, CFuncPointer (rt, args)) ->
      print_ctype out rt;
      fprintf out "(*";
      print_cident out v;
      fprintf out ")(";
      comma_sep print_ctype out args;
      fprintf out ");"
  | CDecl (v, t) ->
      print_ctype out t;
      fprintf out " ";
      print_cident out v;
      fprintf out ";"
  | CAssign (l, e) ->
      print_cexpr out l;
      fprintf out "=";
      print_cexpr out e;
      fprintf out ";"
  | CReturn v ->
      fprintf out "return ";
      print_cident out v;
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
      fprintf out "\n#line %d \"%s\"\n" l f
  | CInclude f ->
      fprintf out "\n#include <%s>\n" f
  | CSwitch (e, options, default) ->
      fprintf out "switch(";
      print_cexpr out e;
      fprintf out "){";
      List.iter (fun (i, b) ->
        fprintf out "case %d:;" i;
        print_cblock out b;
        fprintf out "break;") options;
      begin
        match default with
          | Some b ->
              fprintf out "default:;";
              List.iter (print_cstatement out) b
          | None -> ()
      end;
      fprintf out "}"
  | CLabel l ->
      fprintf out "%s:;" l
  | CGoto l ->
      fprintf out "goto %s;" l


and print_cblock out block =
  fprintf out "{";
  List.iter (print_cstatement out) block;
  fprintf out "}"


and print_cfunc out = function
  | { return_type; args; id; body; loc } ->
      begin
        match loc with
          | Some (lnum, fname) ->
              print_cstatement out (CLoc (lnum, fname))
          | None -> fprintf out "\n"
      end;
      print_ctype out return_type;
      fprintf out " ";
      print_cident out id;
      fprintf out "(";
      comma_sep (fun out (v, t) ->
        print_ctype out t;
        fprintf out " ";
        print_cident out v) out args;
      fprintf out ")";
      print_cblock out body


and print_ctype out = function
  | CUInt -> fprintf out "uintptr_t"
  | CInt -> fprintf out "intptr_t"
  | CFloat -> fprintf out "double"
  | CStr -> fprintf out "char*"
  | CVoid -> fprintf out "void"
  | CValue -> fprintf out "value_type"
(*  | CNamedType i -> fprintf out "%s" (Ident.unique_name i) *)
  | CPointer t ->
      print_ctype out t;
      fprintf out "*"
  (* TODO: probably unify printing types and variables *)
  | CFuncPointer (ret, args) ->
      print_ctype out ret;
      fprintf out "(*)";
      fprintf out "(";
      comma_sep print_ctype out args;
      fprintf out ")"
      (* raise (NotSupported "Function pointers are not supported") *)
  | CClosure _ -> fprintf out "closure_type*"
  | CTypeVar -> fprintf out "variable_type"


and print_ccode out = function
  | { preamble; funcs; main } ->
      List.fold_left (fun () s ->
        print_cstatement out s) () preamble;
      List.fold_left (fun () f ->
        print_cfunc out f;
        fprintf out "\n") () funcs;
      fprintf out "int main()";
      print_cblock out main

