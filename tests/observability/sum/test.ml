external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let rec foldl f acc = function
  | x :: xs -> foldl f (f acc x) xs
  | [] -> acc

let sum = foldl (+) 0

let a = sum [1; 2; 3]
let _ = print_int a; newline ()
