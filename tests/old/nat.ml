type nat =
  | Z
  | S of nat

let rec add x = function
  | Z -> x
  | S y -> add (S x) y

let id = add Z
