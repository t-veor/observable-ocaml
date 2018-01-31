external pint : int -> unit = "print_int"
external pstr : string -> unit = "print_string"
external newl : unit -> unit = "newline"

let length xs =
  let rec helper n = function
    | _ :: xs -> helper (n + 1) xs
    | [] -> n
  in helper 0 xs

let rec map f = function
  | x :: xs -> f x :: map f xs
  | [] -> []

let rec foldl f acc = function
  | x :: xs -> foldl f (f acc x) xs
  | [] -> acc

let rec foldr f acc = function
  | x :: xs -> f x (foldr f acc xs)
  | [] -> acc

let rec filter f = function
  | x :: xs when f x -> x :: filter f xs
  | _ :: xs -> filter f xs
  | [] -> []

let head = function
  | x :: _ -> Some x
  | [] -> None

let tail = function
  | _ :: xs -> Some xs
  | [] -> None

let cons x xs = x :: xs

let append xs ys = foldr cons ys xs

let reverse xs =
  let rec helper acc = function
    | x :: xs -> helper (x :: acc) xs
    | [] -> acc
  in helper [] xs

let sum = foldl (+) 0

let rec print_list = function
  | x :: xs -> pint x; newl (); print_list xs
  | [] -> ()

let rec quicksort = function
  | (x : int) :: xs ->
      let less = filter (fun y -> y < x) xs in
      let more = filter (fun y -> y >= x) xs in
      append (quicksort less) (append [x] (quicksort more))
  | [] -> []

let _ = pstr "reverse/append/sum test"; newl ()
let a = [1; 2; 3; 4]
let b = reverse a
let c = append a b
let _ = print_list c; newl ()
let d = sum c
let _ = pint d; newl (); newl ()

let _ = pstr "closure downcast test"; newl ()
let g = map (+) a
let h = map (fun f -> f 3) g
let _ = print_list h; newl ()

let _ = pstr "lambda test"; newl ()
let h = [3; 6; 1; -2; 46; 23; 19; -1; 30]
let i = quicksort h
let _ = print_list i; newl ()
