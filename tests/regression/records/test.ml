(* Record values test.
 *)
external print_string : string -> unit = "print_string"
external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

type rectangle = {
  x : int;
  y : int;
  width : int;
  height : int;
}

let print_rect r =
  print_string "Rect(x=";
  print_int r.x;
  print_string ", y=";
  print_int r.y;
  print_string ", width=";
  print_int r.width;
  print_string ", height=";
  print_int r.height;
  print_string ")"

let min_int (x : int) y = if x < y then x else y
let max_int (x : int) y = if x > y then x else y

let overlap r1 r2 =
  let x = max_int r1.x r2.x in
  let y = max_int r1.y r2.y in
  let x_ = min_int (r1.x + r1.width) (r2.x + r2.width) in
  let y_ = min_int (r1.y + r1.height) (r2.y + r2.height) in
  let width = x_ - x in
  let height = y_ - y in
  { x; y; width; height }

let a = { x = 0; y = 1; width = 3; height = 4 }
let b = { x = 2; y = -1; width = 2; height = 5 }

let _ =
  print_rect (overlap a b);
  newline ()
