(* driver.ml *)
(* Contains the pipeline to compile a .ml file. *)

open Ident

let implementation ppf sourcefile outputprefix =
  Compmisc.init_path false;
  let module_name = Compenv.module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name module_name;
  let env = Compmisc.initial_env () in
  let (typedtree, coercion) =
    Parsefile.parse_file sourcefile
    |> Typemod.type_implementation sourcefile outputprefix module_name env
  in
  Printtyped.implementation_with_coercion ppf (typedtree, coercion);
  let types = Typecollect.types typedtree in
  let lambda =
    (typedtree, coercion)
    |> Translmod.transl_implementation sourcefile
    |> (fun { Lambda.code = lambda } ->
        Simplif.simplify_lambda sourcefile lambda)
  in 
    Printlambda.lambda ppf lambda;
    Format.fprintf ppf "\n";
    types |> List.rev |> List.map (fun (id, t) ->
      Format.fprintf ppf "(%s/%d, " id.name id.stamp;
      Printtyp.raw_type_expr ppf t;
      Format.fprintf ppf ")\n") |> ignore;
  Ccompile.compile lambda types

