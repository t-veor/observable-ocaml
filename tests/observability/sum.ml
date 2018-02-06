let sum xs =
  let rec go acc = function
    | x :: xs -> go (acc + x) xs
    | [] -> acc
  in go 0 xs

let a = sum [3]
