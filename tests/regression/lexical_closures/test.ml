external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let f = (fun x -> (fun y -> (fun z -> x + y * z)))

let _ = print_int (f 2 3 4); newline ()
