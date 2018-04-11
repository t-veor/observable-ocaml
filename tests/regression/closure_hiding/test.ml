external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let add_five_numbers a b c d e = a + b + c + d + e

let rec map f = function
  | x :: xs -> f x :: map f xs
  | [] -> []

let rec print_list = function
  | x :: xs -> print_int x; newline (); print_list xs
  | [] -> ()

let a = [1; 2; 3; 4; 5]
let b = map add_five_numbers a
let c = map (fun f -> f 1) b
let d = map (fun f -> f 7 9) c
let e = map (fun f -> f (-2)) d
let _ = print_list e
