external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let apply f x = f x

let print_sum x y = print_int (x + y)

let _ = apply print_sum 1 2
