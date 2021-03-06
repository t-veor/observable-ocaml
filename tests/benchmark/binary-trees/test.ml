(* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * Contributed by Troestler Christophe
 * Modified by Fabrice Le Fessant
 * *reset*
 *)

external print_string : string -> unit = "print_string"
external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

type 'a tree = Empty | Node of 'a tree * 'a tree

let rec make d =
(* if d = 0 then Empty *)
  if d = 0 then Node(Empty, Empty)
  else let d = d - 1 in Node(make d, make d)

let rec check = function Empty -> 0 | Node(l, r) -> 1 + check l + check r

let min_depth = 4
let max_depth = 19
             (* (let n = try int_of_string(Array.get Sys.argv 1) with _ -> 10 in
                 max (min_depth + 2) n) *)
let stretch_depth = max_depth + 1

let () =
  (* Gc.set { (Gc.get()) with Gc.minor_heap_size = 1024 * 1024; max_overhead = -1; }; *)
  let c = check (make stretch_depth) in
  print_string "strech tree of depth "; print_int stretch_depth;
  print_string "\t check: "; print_int c;
  newline ()
  (* Printf.printf "stretch tree of depth %i\t check: %i\n" stretch_depth c *)

let long_lived_tree = make max_depth

let rec loop_depths d =
  for i = 0 to  ((max_depth - d) / 2 + 1) - 1 do
    let d = d + i * 2 in
    let niter = 1 lsl (max_depth - d + min_depth) in
    let c = ref 0 in
      for i = 1 to niter do c := !c + check(make d) done;
      print_int niter;
      print_string "\t trees of depth ";
      print_int d;
      print_string "\t check: ";
      print_int !c;
      newline ()
      (* Printf.printf "%i\t trees of depth %i\t check: %i\n" niter d !c; *)
  done

let () =
  (* flush stdout; *)
  loop_depths min_depth;
  print_string "long lived tree of depth ";
  print_int max_depth;
  print_string "\t check: ";
  print_int (check long_lived_tree);
  newline ()
  (*
  Printf.printf "long lived tree of depth %i\t check: %i\n"
    max_depth (check long_lived_tree)
  *)
