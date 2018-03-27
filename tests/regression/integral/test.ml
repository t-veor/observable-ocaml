external print_string : string -> unit = "print_string"
external newline : unit -> unit = "newline"

let apply f x = f x

let is_empty = function
  | [] -> true
  | _  -> false

let is_one = function
  | 1 -> true
  | _ -> false

let _ =
  if apply is_empty []
  then print_string "empty"
  else print_string "nonempty";
  newline ();
  if apply is_one 1
  then print_string "one"
  else print_string "not one";
  newline ()
