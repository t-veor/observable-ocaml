(* Imperative test.
 * Tests that the imperative features of OCaml are translated
 * correctly into C code.
 *)

external print_string : string -> unit = "print_string"
external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let fib n =
  let n = ref n in
  let a = ref 0 in
  let b = ref 1 in

  while !n <> 0 do
    let temp = !a in
    a := !b;
    b := temp + !b;
    n := !n - 1;
  done;
  !b

let _ =
  print_string "for loop test\n";
  for i = 0 to 10 do
    print_int i; newline ();
  done;
  newline ();

  print_string "while loop test\n";
  let x = ref 0 in
  while !x < 10 do
    print_int (fib !x); newline ();
    x := !x + 1;
  done
