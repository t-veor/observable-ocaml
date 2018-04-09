let print_int = Printf.printf "%i"
let newline () = Printf.printf "\n"

let next xs =
  let rec go = function
    | l :: (c :: r :: _ as xs) ->
        l lxor (c lor r) :: go xs
    | l :: (c :: [] as xs) ->
        l lxor c :: go xs
    | l :: [] -> l :: []
    | [] -> []
  in
  match xs with
    | (c :: r :: _ as xs) -> c :: c lor r :: go xs
    | (r :: [] as xs) -> r :: r :: go xs
    | [] -> []

let niter = 8000

let rec iterate n f x =
  match n with
    | 0 -> x
    | n -> iterate (n - 1) f (f x)

let rec print_ints = function
  | x :: xs -> print_int x; print_ints xs
  | [] -> ()

let x = iterate niter next [1]
let _ = print_ints x; newline ()
