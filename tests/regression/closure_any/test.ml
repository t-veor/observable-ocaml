external print_string : string -> unit = "print_string"
external newline : unit -> unit = "newline"

let rev_apply x f = f x

let is_empty = function
  | [] -> true
  | _  -> false

let is_one = function
  | 1 -> true
  | _ -> false

let a = rev_apply ([] : int list)
let b = rev_apply 1

let _ =
  if a is_empty
  then print_string "empty"
  else print_string "nonempty";
  newline ();
  if b is_one
  then print_string "one"
  else print_string "not one";
  newline ()
