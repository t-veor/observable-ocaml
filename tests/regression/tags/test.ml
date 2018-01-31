(* Tag test.
 * Tests that block tags are being generated correctly, and that
 * switches compile to the correct C equivalents.
 *)

external print_string : string -> unit = "print_string"
external newline : unit -> unit = "newline"

type tag_test =
  | A of int
  | B of float
  | C of string
  | D
  | E
  | F

let values = [A 1; B 2.0; C "three"; D; E; F]

(* Should compile to a single if statement. *)
let switch1 = function
  | D _ -> "D"
  | _ -> "not D"

(* Should compile into a single isint check *)
let switch2 = function
  | A _ | B _ | C _ ->
      "A, B, or C"
  | D | E | F ->
      "D, E, or F"

(* Should compile into a full switch statement *)
let switch3 = function
  | A _ -> "A"
  | B _ -> "B"
  | C _ -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"

(* Should compile into a switch statement with a goto *)
let switch4 = function
  | A _ -> "A"
  | C _ -> "C"
  | D -> "D"
  | _ -> "B, E, or F"

let rec iter f = function
  | x :: xs -> print_string (f x); newline (); iter f xs
  | [] -> ()

let _ =
  print_string "test #1\n";
  iter switch1 values;
  newline ();
  print_string "test #2\n";
  iter switch2 values;
  newline ();
  print_string "test #3\n";
  iter switch3 values;
  newline ();
  print_string "test #4\n";
  iter switch4 values
