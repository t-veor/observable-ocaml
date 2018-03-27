(* typecollect.ml *)
(* Scrapes a lambda to obtain types for each variable. *)

open Lambda
open Types
open Typelib
open Primitive

module IdentHash = Hashtbl.Make(struct
  type t = Ident.t
  let equal = Ident.same
  let hash = Ident.hash
end)

module ExternHash = Hashtbl.Make(struct
  type t = string
  let equal x y = x = y
  let hash = Hashtbl.hash
end)

let string_of_ctype t =
  let strbuf = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer strbuf in
  Cprint.print_ctype formatter t;
  Format.pp_print_flush formatter ();
  Buffer.contents strbuf

let set_type t id t1 =
  try
    let t2 = IdentHash.find t id in
    if t1 <> t2 then
      failwith (Printf.sprintf
        "Types for %s disagree (%s, %s)" (Ident.unique_name id)
        (string_of_ctype t1) (string_of_ctype t2))
    else ()
  with Not_found ->
    Printf.printf "Type of %s is %s\n" (Ident.unique_name id)
      (string_of_ctype t1);
    IdentHash.add t id t1

let get_type t id =
  try
    IdentHash.find t id
  with Not_found ->
    Printf.eprintf "Warning: type of %s not found, replacing with any_type\n"
      (Ident.unique_name id);
    Ccode.CAnyType

let set_extern_id e prim_name t1 =
  try
    let t2 = ExternHash.find e prim_name in
    if t1 <> t2 then
      failwith (Printf.sprintf
        "Types for external %s disagree (%s, %s)" prim_name
        (string_of_ctype t1) (string_of_ctype t2))
    else ()
  with Not_found ->
    Printf.printf "Type of external %s is %s\n" prim_name
      (string_of_ctype t1);
    ExternHash.add e prim_name t1

let scrape lam =
  let t = IdentHash.create 16 in
  let e = ExternHash.create 16 in
  let rec scrape_helper lam = iter (fun lam ->
      begin
        match lam with
          | Levent (_, ev) ->
              let env = Envaux.env_from_summary ev.lev_env Subst.identity in
              Env.fold_values (fun str path val_desc acc ->
                match path with
                  | Path.Pident id -> (
                      let t1 = comp_type val_desc.val_type in
                      set_type t id t1;
                      match val_desc.val_kind with
                        | Val_prim { prim_name } ->
                            set_extern_id e prim_name t1
                        | _ -> ()
                      )
                  | _ -> ()
              ) None env ()
          | _ -> ()
      end;
      scrape_helper lam
    ) lam in
  scrape_helper lam;
  (t, e)
