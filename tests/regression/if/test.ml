(* If statement test.
 *)

external print_string : string -> unit = "print_string"

let is_zero = function
  | 0 -> true
  | _ -> false

let _ =
  if is_zero 2 then
    print_string "2 is zero\n"
  else
    print_string "2 is not zero\n"
