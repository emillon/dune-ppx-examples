open Ppxlib

let extender_name = "env"

let extract =
  let open Ast_pattern in
  single_expr_payload (estring __)

let expander ~ctxt env_var_name =
  let open Ast_builder.Make (struct
    let loc = Expansion_context.Extension.extension_point_loc ctxt
  end) in
  match Sys.getenv_opt env_var_name with
  | None ->
      pexp_extension
        (Location.error_extensionf ~loc "Variable %s is not set" env_var_name)
  | Some env_var_value -> estring env_var_value

let extender =
  Extension.V3.declare extender_name Extension.Context.expression extract
    expander

let rule = Context_free.Rule.extension extender
let () = Driver.register_transformation ~rules:[ rule ] extender_name
