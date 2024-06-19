open Ppxlib

let generate_impl_for_one (decl : Parsetree.type_declaration) =
  match decl.ptype_kind with
  | Ptype_variant cds ->
      let open Ast_builder.Default in
      let loc = decl.ptype_loc in
      let exprs = List.map (fun cd -> econstruct cd None) cds in
      let every_name = Printf.sprintf "every_%s" decl.ptype_name.txt in
      let pat = ppat_var ~loc (Loc.make ~loc every_name) in
      let expr = elist ~loc exprs in
      pstr_value_list ~loc Nonrecursive [ value_binding ~loc ~pat ~expr ]
  | _ -> (* TODO error reporting *) assert false

let generate_impl ~ctxt:_ (_rec, decls) =
  List.concat_map generate_impl_for_one decls

let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl
let deriving_every = Deriving.add "every" ~str_type_decl
