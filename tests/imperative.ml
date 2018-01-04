external pint : int -> unit = "print_int"
external newl : unit -> unit = "newline"

let head : int list -> int = function
  | x :: xs -> x
  | [] -> 0

let x = ref [0]
let () =
  while head !x < 10 do
  pint (head !x);
  newl ();
  x := [head !x + 1]
done
