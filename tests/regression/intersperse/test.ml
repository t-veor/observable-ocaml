(* Function overapplication test.
 *)

external print_string : string -> unit = "print_string"
external newline : unit -> unit = "newline"

let intersperse x =
  let rec go = function
    | [y] -> [y]
    | [] -> []
    | y :: ys -> y :: x :: go ys
  in go

let add_newlines = intersperse "\n"

let rec print_list = function
  | x :: xs -> print_string x; print_list xs
  | [] -> ()

let a = ["hello"; "there"; "how"; "are"; "you"]
let _ =  print_list (add_newlines a); newline ()
