(* Closures test.
 *)
external print_int : int -> unit = "print_int"
external print_float : float -> unit = "print_double"
external newline : unit -> unit = "newline"

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
  | x :: xs -> print_int x; newline (); print_list xs
  | [] -> ()

let rec print_listf = function
  | x :: xs -> print_float x; newline (); print_listf xs
  | [] -> ()

let plus a b = a + b
let fplus a b = a +. b

let a = [1; 2; 3; 4]
let b = (+) 10
let c = map b a
let _ = print_int (sum c); newline (); print_list c

let a = [0.1; 0.2; 0.3; 0.4]
let b = map (+.) a
let c = map (fun f -> f 0.1) b
let _ = print_float (sumf c); newline (); print_listf c
