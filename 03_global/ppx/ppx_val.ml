open Ppxlib

let rewrite vd vb =
  let open Ast_builder.Make (struct
    let loc = vd.pval_loc
  end) in
  let me = pmod_structure [ pstr_value Nonrecursive [ vb ] ] in
  let mt = pmty_signature [ psig_value vd ] in
  [%stri include ([%m me] : [%m mt])]

let impl str =
  let previous_val = ref None in
  List.filter_map
    (fun si ->
      match (!previous_val, si.pstr_desc) with
      | None, Pstr_primitive ({ pval_prim = []; _ } as vd) ->
          previous_val := Some vd;
          None
      | Some vd, Pstr_value (Nonrecursive, [ vb ]) ->
          previous_val := None;
          Some (rewrite vd vb)
      | _ -> Some si)
    str

let () = Driver.register_transformation ~impl "ppx_val"
