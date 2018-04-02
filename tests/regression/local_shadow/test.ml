external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let f x y =
  let z = ref x in
  let x = x + 1 in
  let y = y + x in
  for x = 0 to x do
    z := !z + x;
  done;
  !z + y

let _ = print_int (f 20 9); newline ()
