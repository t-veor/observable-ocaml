external pstr : string -> unit = "print_string"
external newl : unit -> unit = "newline"

type test_union =
  | A of int * int
  | B of float
  | C of string
  | D
  | E
  | F
  | G
  | H

let a = A (1, 1)
let b = B 2.0
let c = C "three"
let d = D
let e = E
let f = F
let g = G
let h = H

let typeof = function
  | A _ -> "a"
  | B _ -> "b"
  | C x -> x
  | D -> "d"
  | _ -> "unknown"


let _ =
  pstr (typeof a); newl ();
  pstr (typeof b); newl ();
  pstr (typeof c); newl ();
  pstr (typeof d); newl ();
  pstr (typeof e); newl ();
  pstr (typeof f); newl ();
  pstr (typeof g); newl ();
  pstr (typeof h); newl ()
