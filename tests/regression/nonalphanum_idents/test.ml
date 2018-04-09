external s' : string -> unit = "print_string"
external i' : int -> unit = "print_int"
external n' : unit -> unit = "newline"

let start__ = 0
let end_ = 100

let result' = ref 0

let (+!) r x = r := !r + x

let _ = for i = start__ to end_ do
  result' +! i
done;
s' "result: "; i' !result'; n' ()
