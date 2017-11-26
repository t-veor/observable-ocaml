(* main.ml *)
(* Entry point into the compiler. *)

open Ident

let ppf = Format.std_formatter

let sourcefile = "tests/simple.ml"
let outputprefix = "tests/simple"

let main () = 
  try
    let ctx = Driver.implementation ppf sourcefile outputprefix in
    Cprint.print_ccontext stdout ctx
  with x ->
    Location.report_exception ppf x;
    exit 2

let _ = main ()
