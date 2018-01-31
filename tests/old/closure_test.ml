external pint : int -> unit = "print_int"
external pfloat : float -> unit = "print_double"
external newl : unit -> unit = "newline"

let rec map f = function
  | x :: xs -> f x :: map f xs
  | [] -> []

let rec sum = function
  | x :: xs -> x + sum xs
  | [] -> 0

let rec sumf = function
  | x :: xs -> x +. sumf xs
  | [] -> 0.0

let rec print_list = function
  | x :: xs -> pint x; newl (); print_list xs
  | [] -> ()

let rec print_listf = function
  | x :: xs -> pfloat x; newl (); print_listf xs
  | [] -> ()

let plus a b = a + b
let fplus a b = a +. b

let a = [1; 2; 3; 4]
let b = plus 10
let c = map b a
let _ = pint (sum c); newl (); print_list c

let a = [1.0; 2.0; 3.0; 4.0]
let b = fplus 10.0
let c = map b a
let _ = pfloat (sumf c); newl (); print_listf c
