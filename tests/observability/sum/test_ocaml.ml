let print_int = Printf.printf "%i"
let newline () = Printf.printf "\n"

let rec foldl f acc = function
  | x :: xs -> foldl f (f acc x) xs
  | [] -> acc

let sum = foldl (+) 0

let a = sum [1; 2; 3]
let _ = print_int a; newline ()
