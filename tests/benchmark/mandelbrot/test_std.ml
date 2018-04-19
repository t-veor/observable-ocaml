(* Adapted from
 * https://benchmarksgame.alioth.debian.org/u64q/program.php?test=mandelbrot&lang=ocaml&id=6
 *)

let print_string s = Printf.printf "%s" s
let print_int i = Printf.printf "%d" i
let newline () = Printf.printf "\n"
let print_byte i = output_byte stdout i

let niter = 50
let limit = 4.0
let w = 3000
let h = w

let _ =
  let fw = float w /. 2. and fh = float h/. 2. in
  print_string "P4\n"; print_int w; print_string " "; print_int h; newline ();
  let red_h = h - 1 and red_w = w - 1 and byte = ref 0 in
  for y = 0 to red_h do
    let ci = float y /. fh -. 1. in
    for x = 0 to red_w do
      let cr = float x /. fw -. 1.5
      and zr = ref 0. and zi = ref 0. and trmti = ref 0. and n = ref 0 in
      let cond = ref true in
      while !cond do
        zi := 2. *. !zr *. !zi +. ci;
        zr := !trmti +. cr;
        let tr = !zr *. !zr and ti = !zi *. !zi in
        if tr +. ti > limit then begin
          byte := !byte lsl 1;
          cond := false
        end else if n := !n + 1; !n = niter then begin
          byte := (!byte lsl 1) lor 0x01;
          cond := false
        end else
          trmti := tr -. ti
      done;
      if x mod 8 = 7 then print_byte !byte
    done;
    let rem = w mod 8 in
    if rem != 0 then (* the row doesnt divide evenly by 8 *)
      print_byte (!byte lsl (8 - rem)) (* output last few bits *)
  done
