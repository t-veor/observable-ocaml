external print_string : string -> unit = "print_string"
external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let print_option = function
  | Some x -> print_string "Some "; print_string x; newline ()
  | None -> print_string "None\n"

type 'a tree =
  | Node of int * 'a * 'a tree * 'a tree
  | Leaf

let rec insert k v = function
  | Node (k', v', l, r) ->
      if k < k' then Node (k', v', insert k v l, r)
      else if k > k' then Node (k', v', l, insert k v r)
      else Node (k', v, l, r)
  | Leaf -> Node (k, v, Leaf, Leaf)

let rec lookup k = function
  | Node (k', v, l, r) ->
      if k < k' then lookup k l
      else if k > k' then lookup k r
      else Some v
  | Leaf -> None

let rec print_inorder = function
  | Node (_, v, l, r) ->
      print_inorder l;
      print_string v;
      print_inorder r
  | Leaf -> ()

let x = Leaf
  |> insert 0 "a"
  |> insert 1 "b"
  |> insert (-1) "c"
  |> insert 0 "d"
  |> insert 5 "e"
  |> insert (-4) "f"
  |> insert 19 "g"
  |> insert (-10) "h"
  |> insert 19 "i"
  |> insert 2 "j"
  |> insert (-3) "k"

let _ =
  print_int 2; print_string ": "; print_option (lookup 2 x);
  print_int (-10); print_string ": "; print_option (lookup (-10) x);
  print_int 19; print_string ": "; print_option (lookup 19 x);
  print_int 100; print_string ": "; print_option (lookup 100 x);
  print_inorder x;
  newline ()
