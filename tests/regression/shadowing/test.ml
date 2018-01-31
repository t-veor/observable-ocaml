(* Shadowing test.
 *)

external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let f x = x + 1
let f x = f x + 1
let _ = print_int (f 1); newline ()
