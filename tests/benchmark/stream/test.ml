external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

type 'a stream = Cell of 'a * (unit -> 'a stream)

let hd (Cell (x, _)) = x
let tl (Cell (_, g)) = g ()
let cons x xs = Cell (x, (fun () -> xs))

let rec from n =
  Cell (n, (fun () -> from (n + 1)))

let rec map f (Cell (x, g)) =
  Cell (f x, (fun () -> map f (g ())))

let rec filter p (Cell (x, g)) =
  if p x then
    Cell (x, (fun () -> filter p (g ())))
  else
    filter p (g ())

let rec fold_while f acc p (Cell (x, g)) =
  if p x then
    fold_while f (f acc x) p (g ())
  else
    acc

let rec take n (Cell (x, g)) =
  if n = 0 then
    []
  else
    x :: take (n - 1) (g ())

let rec take_while p (Cell (x, g)) =
  if p x then
    x :: take_while p (g ())
  else
    []

let fib_stream =
  let rec fib a b = Cell (a, (fun () -> fib b (a + b)))
in fib 0 1

let limit = 3000000000
let niter = 1000000

let _ = for i = 1 to niter do
  fib_stream
  |> filter (fun x -> x mod 2 = 0)
  |> fold_while (+) 0 ((>) limit)
  |> print_int;
  newline ()
done
