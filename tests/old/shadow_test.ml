let f x = x + 1

let f x = f x + 1

let _ = Printf.printf "%d\n" (f 1)
