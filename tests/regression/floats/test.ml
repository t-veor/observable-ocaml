external print_float : float -> unit = "print_double"
external newline : unit -> unit = "newline"

let a = 2.0
let b = 3.0

let c = a +. b
let _ = print_float c; newline ()

let d = ref 0.

let _ =
  for i = 1 to 10 do
    d := !d +. (float_of_int i *. 0.1)
  done;
  print_float !d; newline ()
