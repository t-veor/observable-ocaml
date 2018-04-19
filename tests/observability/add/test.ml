let rec add x y =
  if y = 0 then x
  else add (x + 1) (y - 1)
