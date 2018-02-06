(* Finds the longest collatz chain from 1 to 1,000,000.
 *)

let print_string s = Printf.printf "%s" s
let print_int i = Printf.printf "%d" i
let newline () = Printf.printf "\n"

let collatz n =
  let rec helper m = function
    | 1 -> m
    | n when n mod 2 = 1 -> helper (m+1) (3*n+1)
    | n -> helper (m+1) (n/2)
  in helper 0 n

let _ =
  let max = ref (0, 0) in
  for i = 1 to 1000000 do
    let _, b = !max in
    let count = collatz i in
    if count > b then
      max := (i, count)
    else
      ()
  done;
  let (a, b) = !max in
  print_int a; print_string ", "; print_int b; newline ()
