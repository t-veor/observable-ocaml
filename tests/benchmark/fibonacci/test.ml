(* Uses the naive 2^n algorithm to calculate terms in a fibonacci seqence. *)

external print_string : string -> unit = "print_string"
external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

let _ =
  for i = 0 to 39 do
    print_int (fib i);
    newline ()
  done
