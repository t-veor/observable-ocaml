external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let trace x = print_int x; newline (); x

let i = 10
let _ = for i = trace 0 to trace i do
  ignore @@ trace i;
done
