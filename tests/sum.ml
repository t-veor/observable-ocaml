external pint : int -> unit = "print_int"

let rec sum = function
  | [] -> 0
  | x::xs -> x + sum xs

let () = pint (sum [1; 2; 3; 4])
