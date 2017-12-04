(* main.ml *)
(* Entry point into the compiler. *)

open Ident

let ppf = Format.std_formatter

let sourcefile = "tests/simple.ml"
let outputprefix = "tests/simple"

let main () = 
  (* Force Clflags.debug to be true *)
  Clflags.debug := true;
  let code = Driver.implementation ppf sourcefile outputprefix in
  Cprint.print_ccode ppf code

let _ = main ()
