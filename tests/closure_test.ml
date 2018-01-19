external pint : int -> unit = "print_int"
external newl : unit -> unit = "newline"

let rec map f = function
  | x :: xs -> f x :: map f xs
  | [] -> []

let rec sum = function
  | x :: xs -> x + sum xs
  | [] -> 0

let rec print_list = function
  | x :: xs -> pint x; newl (); print_list xs
  | [] -> ()

let plus a b = a + b

let a = [1; 2; 3; 4]
let b = plus 10
let c = map b a
let _ = pint (sum c); newl (); print_list c
