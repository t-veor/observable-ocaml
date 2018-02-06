(* Sieve of Eratosthenes implementation. *)

external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let range min max =
  let rec go acc curr =
    if curr < min then acc
    else go (curr :: acc) (curr - 1)
  in go [] max

let rec reverse_append xs ys = match xs with
  | x :: xs -> reverse_append xs (x :: ys)
  | [] -> ys

let filter f xs =
  let rec go xs = function
    | y :: ys when f y -> go (y :: xs) ys
    | y :: ys -> go xs ys
    | [] -> xs
  in reverse_append (go [] xs) []

let primes max =
  let rec sieve acc remaining =
    match remaining with
      | x :: xs when x * x <= max ->
          let rest = filter (fun y -> y mod x <> 0) xs in
          sieve (x :: acc) rest
      | xs -> reverse_append acc xs
  in sieve [] (range 2 max)

let rec print_ints = function
  | x :: xs -> print_int x; newline (); print_ints xs
  | [] -> ()

let _ =
  let ps = primes 1_000_000 in
  print_ints ps
