(*-----------------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>
   Distributed under the MIT license. See terms at the end of this file.
  -----------------------------------------------------------------------------*)

open Ppxlib
open Ast_helper
open Ast_builder.Default

let upper_snake s =
  String.split_on_char '_' s
  |> List.map String.capitalize_ascii
  |> String.concat ""

let mangle_name_schema =
  let base = "schema_typ" in
  function "t" -> base | x -> x ^ "_" ^ base

let mangle_name_arg =
  let base = "arg_typ" in
  function "t" -> base | x -> x ^ "_" ^ base

let mangle_longident_schema = function
  | Lident l -> Lident (mangle_name_schema l)
  | Ldot (l, n) -> Ldot (l, mangle_name_schema n)
  | _ -> assert false

let mangle_longident_arg = function
  | Lident l -> Lident (mangle_name_arg l)
  | Ldot (l, n) -> Ldot (l, mangle_name_arg n)
  | _ -> assert false

let longident_loc_from_label lbl = { txt = Lident lbl.txt; loc = lbl.loc }

(* <><><> Attributes <><><> *)

module Attrs = struct
  let resolver =
    Attribute.declare "graphql.schema.resolver"
      Attribute.Context.label_declaration
      Ast_pattern.(single_expr_payload __)
      (fun x -> x)

  let schema_doc =
    Attribute.declare "graphql.schema.doc" Attribute.Context.label_declaration
      Ast_pattern.(single_expr_payload __)
      (fun x -> x)

  let constr_doc =
    Attribute.declare "graphql.schema.doc"
      Attribute.Context.constructor_declaration
      Ast_pattern.(single_expr_payload __)
      (fun x -> x)
end

(* <><><> Schemas <><><> *)

let rec type_to_schema ?(name = "") ?(non_null = true) typ =
  let loc = typ.ptyp_loc in
  let fn =
    if non_null then [%expr Graphql_lwt.Schema.non_null] else [%expr Fun.id]
  in
  match typ with
  | [%type: int] -> [%expr [%e fn] Graphql_lwt.Schema.int]
  | [%type: float] -> [%expr [%e fn] Graphql_lwt.Schema.float]
  | [%type: string] -> [%expr [%e fn] Graphql_lwt.Schema.string]
  | [%type: bool] -> [%expr [%e fn] Graphql_lwt.Schema.bool]
  (* | [%type: char] -> [%expr fun (x : char) -> `String (String.make 1 x)] *)
  | [%type: [%t? typ] list] ->
      [%expr [%e fn] (Graphql_lwt.Schema.list [%e type_to_schema typ])]
  | [%type: [%t? typ] option] -> [%expr [%e type_to_schema ~non_null:false typ]]
  | { ptyp_desc = Ptyp_constr ({ txt; _ }, _args); _ } ->
      let name = mangle_longident_schema txt |> Longident.name in
      [%expr [%e fn] [%e evar ~loc name]]
  | { ptyp_desc = Ptyp_var name; _ } ->
      Location.raise_errorf "Cannot derive schema for ptyp_var %s" name
  | { ptyp_desc = Ptyp_poly _; _ } ->
      Location.raise_errorf "Polymorphic functions not currently supported"
  | { ptyp_desc = Ptyp_tuple _; _ } ->
      Location.raise_errorf "Tuples not currently supported"
  | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
      let enum_values =
        List.fold_left
          (fun expr_acc (field : row_field) ->
            match field.prf_desc with
            | Rtag (label, true, []) ->
                let v = pexp_variant ~loc label.txt None in
                [%expr
                  Graphql_lwt.Schema.enum_value [%e estring ~loc label.txt]
                    ~value:[%e v]
                  :: [%e expr_acc]]
            | _ -> failwith "Not implemented")
          [%expr []] row_fields
      in
      [%expr
        Graphql_lwt.Schema.enum [%e estring ~loc name] ~values:[%e enum_values]]
  | _ ->
      Location.raise_errorf ~loc
        "Cannot derive anything for this type (typ_to_schema)"

let record_to_schema ~loc ~label fields =
  let label = estring ~loc label in
  let fields =
    List.fold_left
      (fun expr_acc (field : label_declaration) ->
        let var_name f = f ~loc "p" in
        let field_name = field.pld_name.txt in
        let accessor_name : longident_loc =
          { txt = Lident field_name; loc = field.pld_name.loc }
        in
        let accessor = pexp_field ~loc (var_name evar) accessor_name in
        let accessor_func =
          match Attribute.get Attrs.resolver field with
          | Some expr -> expr
          | None -> [%expr fun _ [%p var_name pvar] -> [%e accessor]]
        in
        let field_doc =
          match Attribute.get Attrs.schema_doc field with
          | Some doc -> [%expr Some [%e doc]]
          | None -> [%expr None]
        in
        [%expr
          Graphql_lwt.Schema.field ?doc:[%e field_doc]
            [%e estring ~loc field_name]
            ~typ:[%e type_to_schema field.pld_type]
            ~args:[] ~resolve:[%e accessor_func]
          :: [%e expr_acc]])
      [%expr []] fields
  in
  [%expr Graphql_lwt.Schema.obj [%e label] ~fields:[%e fields]]

(* <><><> Arguments <><><> *)

let rec type_to_arg ?(name = "") ?(non_null = true) typ =
  let loc = typ.ptyp_loc in
  let fn =
    if non_null then [%expr Graphql_lwt.Schema.Arg.non_null] else [%expr Fun.id]
  in
  match typ with
  | [%type: int] -> [%expr [%e fn] Graphql_lwt.Schema.Arg.int]
  | [%type: float] -> [%expr [%e fn] Graphql_lwt.Schema.Arg.float]
  | [%type: string] -> [%expr [%e fn] Graphql_lwt.Schema.Arg.string]
  | [%type: bool] -> [%expr [%e fn] Graphql_lwt.Schema.Arg.bool]
  (* | [%type: char] -> [%expr fun (x : char) -> `String (String.make 1 x)] *)
  | [%type: [%t? typ] list] ->
      [%expr [%e fn] (Graphql_lwt.Schema.Arg.list [%e type_to_arg typ])]
  | [%type: [%t? typ] option] -> [%expr [%e type_to_arg ~non_null:false typ]]
  | { ptyp_desc = Ptyp_constr ({ txt; _ }, _args); _ } ->
      let name = mangle_longident_arg txt |> Longident.name in
      [%expr [%e fn] [%e evar ~loc name]]
  | { ptyp_desc = Ptyp_var name; _ } ->
      Location.raise_errorf "Cannot derive arg for ptyp_var %s" name
  | { ptyp_desc = Ptyp_poly _; _ } ->
      Location.raise_errorf "Polymorphic functions not currently supported"
  | { ptyp_desc = Ptyp_tuple _; _ } ->
      Location.raise_errorf "Tuples not currently supported"
  | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
      let enum_values =
        List.fold_left
          (fun expr_acc (field : row_field) ->
            match field.prf_desc with
            | Rtag (label, true, []) ->
                let v = pexp_variant ~loc label.txt None in
                [%expr
                  Graphql_lwt.Schema.enum_value [%e estring ~loc label.txt]
                    ~value:[%e v]
                  :: [%e expr_acc]]
            | _ -> failwith "Not implemented")
          [%expr []] row_fields
      in
      [%expr
        Graphql_lwt.Schema.Arg.enum [%e estring ~loc name]
          ~values:[%e enum_values]]
  | _ ->
      Location.raise_errorf ~loc
        "Cannot derive anything for this type (typ_to_arg)"

let record_to_arg ~loc ~label fields =
  let label = estring ~loc label in
  let efields =
    List.fold_left
      (fun expr_acc (field : label_declaration) ->
        let field_name = field.pld_name.txt in
        let field_doc =
          match Attribute.get Attrs.schema_doc field with
          | Some doc -> [%expr Some [%e doc]]
          | None -> [%expr None]
        in
        [%expr
          Graphql_lwt.Schema.Arg.arg ?doc:[%e field_doc]
            [%e estring ~loc field_name] ~typ:[%e type_to_arg field.pld_type]
          :: [%e expr_acc]])
      [%expr []] fields
  in
  let record =
    let field_bindings =
      List.map
        (fun lbl ->
          ( longident_loc_from_label lbl.pld_name,
            [%expr [%e evar ~loc lbl.pld_name.txt]] ))
        fields
    in
    pexp_record ~loc field_bindings None
  in
  let ecoerce =
    List.fold_left
      (fun expr_acc (field : label_declaration) ->
        let field_name = pvar ~loc field.pld_name.txt in
        [%expr fun [%p field_name] -> [%e expr_acc]])
      [%expr [%e record]] fields
  in
  [%expr
    Graphql_lwt.Schema.Arg.obj [%e label] ~fields:[%e efields]
      ~coerce:[%e ecoerce]]

let generate_impl_schema ~ctxt (_rec_flag, type_decls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat
    (List.map
       (fun typ_decl ->
         match typ_decl with
         | { ptype_kind = Ptype_variant constructors; ptype_name; _ } ->
             let txt = mangle_name_schema ptype_name.txt in
             let p =
               ppat_constraint ~loc
                 (ppat_var ~loc { loc; txt })
                 [%type: (ctx, _) Graphql_lwt.Schema.typ]
             in
             let enum_values =
               List.fold_left
                 (fun expr_acc (constr : constructor_declaration) ->
                   let v =
                     pexp_construct ~loc
                       (longident_loc_from_label constr.pcd_name)
                       None
                   in
                   let constr_doc =
                     match Attribute.get Attrs.constr_doc constr with
                     | Some doc -> [%expr Some [%e doc]]
                     | None -> [%expr None]
                   in
                   [%expr
                     Graphql_lwt.Schema.enum_value
                       [%e estring ~loc constr.pcd_name.txt]
                       ?doc:[%e constr_doc] ~value:[%e v]
                     :: [%e expr_acc]])
                 [%expr []] constructors
             in
             [
               pstr_value ~loc Nonrecursive
                 [
                   Vb.mk p
                     [%expr
                       Graphql_lwt.Schema.enum
                         [%e estring ~loc (upper_snake ptype_name.txt)]
                         ~values:[%e enum_values]];
                 ];
             ]
         | { ptype_kind = Ptype_record fields; ptype_loc = _; ptype_name; _ } ->
             let txt = mangle_name_schema ptype_name.txt in
             let p =
               ppat_constraint ~loc
                 (ppat_var ~loc { loc; txt })
                 [%type: (ctx, _) Graphql_lwt.Schema.typ]
             in
             [
               pstr_value ~loc Nonrecursive
                 [
                   Vb.mk p
                     (record_to_schema ~loc
                        ~label:(upper_snake ptype_name.txt)
                        fields);
                 ];
             ]
         | {
          ptype_kind = Ptype_abstract;
          ptype_manifest = Some manifest;
          ptype_name;
          _;
         } ->
             let txt = mangle_name_schema ptype_name.txt in
             let p =
               ppat_constraint ~loc
                 (ppat_var ~loc { loc; txt })
                 [%type: (ctx, _) Graphql_lwt.Schema.typ]
             in
             [
               pstr_value ~loc Nonrecursive
                 [
                   Vb.mk p
                     (type_to_schema
                        ~name:(upper_snake ptype_name.txt)
                        ~non_null:false manifest);
                 ];
             ]
         | { ptype_kind = Ptype_abstract; ptype_manifest = None; _ } ->
             Location.raise_errorf ~loc
               "Abstract types with no manifest are currently unsupported for \
                generating GraphQL schemas."
         | { ptype_kind = Ptype_open; _ } ->
             Location.raise_errorf ~loc
               "Open types are currently unsupported for generating GraphQL \
                schemas.")
       type_decls)

let generate_impl_arg ~ctxt (_rec_flag, type_decls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat
    (List.map
       (fun typ_decl ->
         match typ_decl with
         | { ptype_kind = Ptype_variant constructors; ptype_name; _ } ->
             let txt = mangle_name_arg ptype_name.txt in
             let lident = longident_loc_from_label typ_decl.ptype_name in
             let p =
               ppat_constraint ~loc
                 (ppat_var ~loc { loc; txt })
                 [%type:
                   [%t ptyp_constr ~loc lident []] option
                   Graphql_lwt.Schema.Arg.arg_typ]
             in
             let enum_values =
               List.fold_left
                 (fun expr_acc (constr : constructor_declaration) ->
                   let v =
                     pexp_construct ~loc
                       (longident_loc_from_label constr.pcd_name)
                       None
                   in
                   let field_doc =
                     match Attribute.get Attrs.constr_doc constr with
                     | Some doc -> [%expr Some [%e doc]]
                     | None -> [%expr None]
                   in
                   [%expr
                     Graphql_lwt.Schema.enum_value
                       [%e estring ~loc constr.pcd_name.txt]
                       ?doc:[%e field_doc] ~value:[%e v]
                     :: [%e expr_acc]])
                 [%expr []] constructors
             in
             [
               pstr_value ~loc Nonrecursive
                 [
                   Vb.mk p
                     [%expr
                       Graphql_lwt.Schema.Arg.enum
                         [%e
                           estring ~loc (upper_snake ptype_name.txt ^ "Input")]
                         ~values:[%e enum_values]];
                 ];
             ]
         | { ptype_kind = Ptype_record fields; ptype_loc = _; ptype_name; _ } ->
             let txt = mangle_name_arg ptype_name.txt in
             let lident = longident_loc_from_label typ_decl.ptype_name in
             let p =
               ppat_constraint ~loc
                 (ppat_var ~loc { loc; txt })
                 [%type:
                   [%t ptyp_constr ~loc lident []] option
                   Graphql_lwt.Schema.Arg.arg_typ]
             in
             [
               pstr_value ~loc Nonrecursive
                 [
                   Vb.mk p
                     (record_to_arg ~loc
                        ~label:(upper_snake ptype_name.txt ^ "Input")
                        fields);
                 ];
             ]
         | {
          ptype_kind = Ptype_abstract;
          ptype_manifest = Some manifest;
          ptype_name;
          _;
         } ->
             let txt = mangle_name_arg ptype_name.txt in
             let lident = longident_loc_from_label typ_decl.ptype_name in
             let p =
               ppat_constraint ~loc
                 (ppat_var ~loc { loc; txt })
                 [%type:
                   [%t ptyp_constr ~loc lident []] option
                   Graphql_lwt.Schema.Arg.arg_typ]
             in
             [
               pstr_value ~loc Nonrecursive
                 [
                   Vb.mk p
                     (type_to_arg ~non_null:false
                        ~name:(upper_snake ptype_name.txt ^ "Input")
                        manifest);
                 ];
             ]
         | { ptype_kind = Ptype_abstract; ptype_manifest = None; _ } ->
             Location.raise_errorf ~loc
               "Abstract types with no manifest are currently unsupported for \
                generating GraphQL arguments."
         | { ptype_kind = Ptype_open; _ } ->
             Location.raise_errorf ~loc
               "Open types are currently unsupported for generating GraphQL \
                arguments.")
       type_decls)

let generate_intf_schema ~ctxt (_rec_flag, type_decls) :
    Ppxlib.Ast.signature_item list =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun typ_decl ->
      match typ_decl with
      | { ptype_kind = Ptype_abstract | Ptype_record _; _ } ->
          let lident = longident_loc_from_label typ_decl.ptype_name in
          [
            psig_value ~loc
              (Val.mk
                 {
                   loc = typ_decl.ptype_name.loc;
                   txt = mangle_name_schema typ_decl.ptype_name.txt;
                 }
                 [%type:
                   ( ctx,
                     [%t ptyp_constr ~loc lident []] option )
                   Graphql_lwt.Schema.typ]);
          ]
      | _ ->
          Location.raise_errorf ~loc
            "Cannot derive anything for this type (intf schema)")
    type_decls
  |> List.concat

let generate_intf_arg ~ctxt (_rec_flag, type_decls) :
    Ppxlib.Ast.signature_item list =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun typ_decl ->
      match typ_decl with
      | { ptype_kind = Ptype_abstract | Ptype_record _; _ } ->
          let lident = longident_loc_from_label typ_decl.ptype_name in
          [
            psig_value ~loc
              (Val.mk
                 {
                   loc = typ_decl.ptype_name.loc;
                   txt = mangle_name_arg typ_decl.ptype_name.txt;
                 }
                 [%type:
                   [%t ptyp_constr ~loc lident []] option
                   Graphql_lwt.Schema.Arg.arg_typ]);
          ]
      | _ ->
          Location.raise_errorf ~loc
            "Cannot derive anything for this type (intf arg)")
    type_decls
  |> List.concat

let impl_generator impl = Deriving.Generator.V2.make_noarg impl
let intf_generator intf = Deriving.Generator.V2.make_noarg intf

let deriver =
  let schema =
    Deriving.add "graphql_schema"
      ~str_type_decl:(impl_generator generate_impl_schema)
      ~sig_type_decl:(intf_generator generate_intf_schema)
  in
  let arg =
    Deriving.add "graphql_arg"
      ~str_type_decl:(impl_generator generate_impl_arg)
      ~sig_type_decl:(intf_generator generate_intf_arg)
  in
  Deriving.add_alias "graphql" [ schema; arg ]

(*-----------------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   THE SOFTWARE.
  -----------------------------------------------------------------------------*)
