let print_int = Printf.printf "%i"
let newline () = Printf.printf "\n"

let limit = 300000000
let niter = 5000000

let rec even_fib a b acc =
  if a > limit then acc
  else
    let acc = (if a mod 2 = 0 then a + acc else acc) in
    even_fib b (a + b) acc

let _ = for i = 0 to niter do
  print_int (even_fib 0 1 0); newline ()
done
