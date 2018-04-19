(* Adapted from
 * http://benchmarksgame.alioth.debian.org/u64q/program.php?test=nbody&lang=ocaml&id=1
 *)

external print_float : float -> unit = "print_double"
external newline : unit -> unit = "newline"
external sqrt : float -> float = "sqrt"

let niter = 2500000

let pi = 3.141592653589793
let solar_mass = 4. *. pi *. pi
let days_per_year = 365.24

type planet = { mutable x : float;  mutable y : float;  mutable z : float;
                mutable vx: float;  mutable vy: float;  mutable vz: float;
                mass : float; name : string }

let rec iterate f = function
  | x :: xs -> f x xs; iterate f xs
  | [] -> ()

let advance bodies dt =
  iterate (fun b bs ->
    iterate (fun b_ _ ->
      let dx = b.x -. b_.x  and dy = b.y -. b_.y  and dz = b.z -. b_.z in
      let dist2 = dx *. dx +. dy *. dy +. dz *. dz in
      let mag = dt /. (dist2 *. sqrt(dist2)) in

      b.vx <- b.vx -. dx *. b_.mass *. mag;
      b.vy <- b.vy -. dy *. b_.mass *. mag;
      b.vz <- b.vz -. dz *. b_.mass *. mag;

      b_.vx <- b_.vx +. dx *. b.mass *. mag;
      b_.vy <- b_.vy +. dy *. b.mass *. mag;
      b_.vz <- b_.vz +. dz *. b.mass *. mag;
    ) bs
  ) bodies;
  iterate (fun b _ ->
    b.x <- b.x +. dt *. b.vx;
    b.y <- b.y +. dt *. b.vy;
    b.z <- b.z +. dt *. b.vz;
  ) bodies


let energy bodies =
  let e = ref 0. in
  iterate (fun b bs ->
    e := !e +. 0.5 *. b.mass *. (b.vx *. b.vx +. b.vy *. b.vy +. b.vz *. b.vz);
    iterate (fun b_ _ ->
      let dx = b.x -. b_.x  and dy = b.y -. b_.y  and dz = b.z -. b_.z in
      let distance = sqrt(dx *. dx +. dy *. dy +. dz *. dz) in
      e := !e -. (b.mass *. b_.mass) /. distance
    ) bs
  ) bodies;
  !e


let offset_momentum bodies =
  let px = ref 0. and py = ref 0. and pz = ref 0. in
  iterate (fun b _ ->
    px := !px +. b.vx *. b.mass;
    py := !py +. b.vy *. b.mass;
    pz := !pz +. b.vz *. b.mass;
  ) bodies;
  match bodies with
    | b :: _ ->
        b.vx <- -. !px /. solar_mass;
        b.vy <- -. !py /. solar_mass;
        b.vz <- -. !pz /. solar_mass
    | _ -> ()


let jupiter = { x = 4.84143144246472090e+00;
                y = -1.16032004402742839e+00;
                z = -1.03622044471123109e-01;
                vx = 1.66007664274403694e-03 *. days_per_year;
                vy = 7.69901118419740425e-03 *. days_per_year;
                vz = -6.90460016972063023e-05 *. days_per_year;
                mass = 9.54791938424326609e-04 *. solar_mass;
                name = "jupiter" }

let saturn = { x = 8.34336671824457987e+00;
               y = 4.12479856412430479e+00;
               z = -4.03523417114321381e-01;
               vx = -2.76742510726862411e-03 *. days_per_year;
               vy = 4.99852801234917238e-03 *. days_per_year;
               vz = 2.30417297573763929e-05 *. days_per_year;
               mass = 2.85885980666130812e-04 *. solar_mass;
               name = "saturn" }

let uranus = { x = 1.28943695621391310e+01;
               y = -1.51111514016986312e+01;
               z = -2.23307578892655734e-01;
               vx = 2.96460137564761618e-03 *. days_per_year;
               vy = 2.37847173959480950e-03 *. days_per_year;
               vz = -2.96589568540237556e-05 *. days_per_year;
               mass = 4.36624404335156298e-05 *. solar_mass;
               name = "uranus" }

let neptune = { x = 1.53796971148509165e+01;
                y = -2.59193146099879641e+01;
                z = 1.79258772950371181e-01;
                vx = 2.68067772490389322e-03 *. days_per_year;
                vy = 1.62824170038242295e-03 *. days_per_year;
                vz = -9.51592254519715870e-05 *. days_per_year;
                mass = 5.15138902046611451e-05 *. solar_mass;
                name = "neptune" }

let sun = { x = 0.;  y = 0.;  z = 0.;  vx = 0.;  vy = 0.; vz = 0.;
            mass = solar_mass; name = "sun" }

let bodies = [ sun; jupiter; saturn; uranus; neptune ]

let () =
  let n = niter in
  offset_momentum bodies;
  print_float (energy bodies); newline ();
  for i = 1 to n do advance bodies 0.01 done;
  print_float (energy bodies); newline ()
