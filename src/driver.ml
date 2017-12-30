(* driver.ml *)
(* Contains the pipeline to compile a .ml file. *)

open Ident

let implementation ppf sourcefile outputprefix =
  Compmisc.init_path false;
  let module_name = Compenv.module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name module_name;
  let env = Compmisc.initial_env () in
  let lambda = try
    Parsefile.parse_file sourcefile
    |> Typemod.type_implementation sourcefile outputprefix module_name env
    |> Translmod.transl_implementation sourcefile
    |> (fun { Lambda.code = lambda } ->
        Simplif.simplify_lambda sourcefile lambda)
  with x ->
    Location.report_exception ppf x;
    exit 2
  in
  let types = Typecollect.scrape lambda in
    Printlambda.lambda ppf lambda;
    Format.fprintf ppf "\n";
  Ccompile.comp_code lambda types

