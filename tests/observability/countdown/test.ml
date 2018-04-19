external print_string : string -> unit = "print_string"
external print_int : int -> unit = "print_int"
external newline : unit -> unit = "newline"

let _ =
  print_string "Countdown:";
  for i = 5 downto 0 do
    print_int i; newline ()
  done
