(* Uses the naive 2^n algorithm to calculate terms in a fibonacci seqence. *)

let print_string s = Printf.printf "%s" s
let print_int i = Printf.printf "%d" i
let newline () = Printf.printf "\n"

let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

let _ =
  for i = 0 to 39 do
    print_int (fib i);
    newline ()
  done
