(* Tests for the result and option types, which are part of Pervasives *)

external print_string : string -> unit = "print_string"
external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let print_option = function
  | Some x -> print_string "Some "; print_int x; newline ()
  | None -> print_string "None\n"

let print_result = function
  | Ok x -> print_string "Ok "; print_int x; newline ()
  | Error s -> print_string "Error "; print_string s; newline ()

let _ =
  print_option (Some 2);
  print_option None;
  print_result (Ok 5);
  print_result (Error "example error")
