external pint : int -> unit = "print_int"

let rec fib a b = function
  | 0 -> a
  | n -> fib b (a + b) (n - 1)

let () = let n = fib 0 1 10 in pint n
