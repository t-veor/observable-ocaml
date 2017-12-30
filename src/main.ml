(* main.ml *)
(* Entry point into the compiler. *)

open Ident

let ppf = Format.std_formatter

let sourcefile = ref ""
let outputprefix = ref ""

let speclist = [
  ("-o", Arg.Set_string outputprefix, "The output prefix");
]

let anonymous filename = sourcefile := filename

let usage = "Compiles an OCaml file to C. Options available:"

let main () =
  Arg.parse speclist anonymous usage;
  (* Force Clflags.debug to be true *)
  Clflags.debug := true;
  let code = Driver.implementation ppf !sourcefile !outputprefix in
  let out_chan = open_out (!outputprefix ^ ".c") in
  Cprint.print_ccode (Format.formatter_of_out_channel out_chan) code

let _ = main ()
