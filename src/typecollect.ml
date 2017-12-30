(* typecollect.ml *)
(* Scrapes a lambda to obtain types for each variable. *)

open Lambda
open Types
open Typelib

module IdentHash = Hashtbl.Make(struct
  type t = Ident.t
  let equal = Ident.same
  let hash = Ident.hash
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
    Printf.printf "Warning: type of %s not found, replacing with void*\n"
      (Ident.unique_name id);
    Ccode.CPointer Ccode.CVoid

let scrape lam =
  let t = IdentHash.create 16 in
  let rec scrape_helper t = iter (fun lam ->
      begin
        match lam with
          | Levent (_, ev) ->
              let env = Envaux.env_from_summary ev.lev_env Subst.identity in
              Env.fold_values (fun str path val_desc acc ->
                match path with
                  | Path.Pident id -> (
                      let t1 = comp_type val_desc.val_type in
                      set_type t id t1
                      )
                  | _ -> ()
              ) None env ()
          | _ -> ()
      end;
      scrape_helper t lam
    ) in
  scrape_helper t lam;
  t
