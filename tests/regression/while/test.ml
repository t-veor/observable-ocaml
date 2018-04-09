external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let trace x = print_int x; newline (); x
let x = ref 0

let _ = while trace !x < 10 do
  x := !x + 1
done
