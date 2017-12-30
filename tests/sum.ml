let rec sum = function
  | [] -> 0
  | x::xs -> x + sum xs

let () = print_string (string_of_int (sum [1; 2; 3; 4]))
