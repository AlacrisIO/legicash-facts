open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "rlp"
let raise_errorf = Ppx_deriving.raise_errorf

(* zip2 : ('a list * 'b list) -> ('a * 'b * 'c * 'd) list *)
let zip2 xs ys = List.map2 (fun x y -> (x, y)) xs ys

(* unzip2 : ('a * 'b) list -> ('a list * 'b list) *)
let unzip2 l =
  List.fold_right (fun (v1,v2) (a1,a2) -> (v1::a1, v2::a2)) l ([],[])

(* unzip3 : ('a * 'b * 'c) list -> ('a list * 'b list * 'c list) *)
let unzip3 l =
  List.fold_right (fun (v1,v2,v3) (a1,a2,a3) -> (v1::a1, v2::a2, v3::a3)) l ([],[],[])

(* unzip4 : ('a * 'b * 'c * 'd) list -> ('a list * 'b list * 'c list * 'd list) *)
let unzip4 l =
  List.fold_right (fun (v1,v2,v3,v4) (a1,a2,a3,a4) -> (v1::a1, v2::a2, v3::a3, v4::a4)) l ([],[],[],[])

let mapi2 f l1 l2 =
  List.mapi f (zip2 l1 l2)

(* lams : pat list -> exp -> exp *)
let lams = List.fold_right lam

(* arrows : typ list -> typ -> typ *)
let arrows = List.fold_right (Typ.arrow Nolabel)

let turn_off_warning_this_match_case_is_unused =
  ({loc = !default_loc; txt = "ocaml.warning"},
   PStr[Str.eval (Exp.constant (Const.string "-11"))])

(* Construct expressions for Some and None *)
let exp_some exp = Exp.construct (lid "Some") (Some exp)
let exp_none = Exp.construct (lid "None") None

(* This function should be used as the last case in a match that
   matches an RlpItem tree, where the previous cases might fail
   because they expect a certain shape for the data type. *)
let case_else_data_type_mismatch s =
  Exp.case (pvar "v")
           (app (Exp.ident (lid "raise"))
                [Exp.construct (lid "Ppx_deriving_rlp_runtime_core.Rlp.Rlp_data_type_mismatch")
                               (Some (Exp.tuple [Exp.constant (Const.string s); evar "v"]))])

(* Show a string representation of a type for error messages *)
let string_of_type core_type =
  let bfr = Buffer.create 1 in
  let fmtr = Format.formatter_of_buffer bfr
  in Pprintast.core_type fmtr core_type;
     Format.pp_print_flush fmtr ();
     Buffer.contents bfr

(* ------------------------------------------------
   --- useful helpers from ppx_deriving_yojson  --- *)
(* calls `f` on the contents of a structure *)
let structure f ~options ~path type_ =
  let (pre, vals, post) = f ~options ~path type_ in
  match vals with
  | [] -> pre @ post
  | _  -> pre @ [Str.value ?loc:None Recursive vals] @ post

(* calls `f` on each type declaration in a structure *)
let on_str_decls f ~options ~path type_decls =
  let (pre, vals, post) = unzip3 (List.map (f ~options ~path) type_decls) in
  (List.concat pre, List.concat vals, List.concat post)

(* calls `f` on each type declaration in a signature *)
let on_sig_decls f ~options ~path type_decls =
  List.concat (List.map (f ~options ~path) type_decls)
(* ------------------------------------------------ *)

(* str_to_lid : string loc -> longident loc
   str_to_lid : Ast_helper.str -> Ast_helper.lid *)
let str_to_lid { txt; loc } = { txt = Lident txt; loc = loc }

(* generate_temporaries : string -> 'a list -> string list *)
(* Generates a list of strings based on `base` with different numbers added to the end *)
let generate_temporaries base vs = List.mapi (fun i _ -> base ^ string_of_int i) vs

(* A "bidirectional case" is a tuple of four values:
    * a `pattern` that matches the datatype
    * a `pattern` that matches the RLP tree
    * an `expression` that reconstructs the datatype
    * an `expression` that reconstructs the RLP tree
 *)
type bidirectional_case = (pattern * pattern * expression * expression)

(* case_data_to_rlp : bidirectional_case -> Parsetree.case *)
(* Generates a Parsetree match case for converting from data to an rlp_item *)
let case_data_to_rlp (data_pat, _, _, rlp_exp) = Exp.case data_pat rlp_exp

(* case_rlp_to_data : bidirectional_case -> Parsetree.case *)
(* Generates a Parsetree match case for converting from an rlp_item to data *)
let case_rlp_to_data (_, rlp_pat, data_exp, _) = Exp.case rlp_pat data_exp

(* cases_unzip : bidirectional_case list -> (pat list * pat list * exp list * exp list) *)
(* Produces a tuple of four values:
    * a list of patterns that match pieces of the datatype
    * a list of patterns that match pieces of the RLP tree
    * a list of expressions that reconstruct pieces of the datatype
    * a list of expressions that reconstruct pieces of the RLP tree
  *)
let cases_unzip : bidirectional_case list -> (pattern list * pattern list * expression list * expression list) = unzip4

(* ctor_tup_args : (pat list -> pat) -> pat list -> pat option
   ctor_tup_args : (exp list -> exp) -> exp list -> exp option *)
let ctor_tup_args tuple args =
  match args with
  | []    -> None
  | [arg] -> Some arg
  | args  -> Some (tuple args)

(* ------------------------------------------------ *)

(* supported_pervasives_types
   These are the types which are built-in or in the `Pervasives`
   module,  that are supported directly by functions in
   `ppx_deriving_rlp_runtime.ml`. *)
let supported_pervasives_types =
  ["unit"; "string"; "char"; "int"; "float"; "bool"; "list"; "option"; "result"]

(* supported_module_t_types
   These are modules which provide types named "t", that are
   supported directly by functions in
   `ppx_deriving_rlp_runtime.ml`. Those functions' names are
   prefixed with the uncapitalized version of the module.
   For example the operations on `Z.t` are in functions that
   start with `z_` in `ppx_deriving_rlp_runtime.ml`. *)
let supported_module_t_types =
  ["Z"]

(* ------------------------------------------------ *)

type 'a rlp_ops = { to_rlp_item: 'a;
                    of_rlp_item: 'a;
                    of_rlp_item_opt: 'a;
                    to_rlp: 'a;
                    of_rlp: 'a;
                    of_rlp_opt: 'a;
                    marshal_rlp: 'a;
                    unmarshal_rlp: 'a;
                    unmarshal_rlp_opt: 'a;
                    rlping: 'a }

let rlp_ops_to_list { to_rlp_item; of_rlp_item; of_rlp_item_opt;
                      to_rlp; of_rlp; of_rlp_opt;
                      marshal_rlp; unmarshal_rlp; unmarshal_rlp_opt;
                      rlping }
  =
  [ to_rlp_item; of_rlp_item; of_rlp_item_opt;
    to_rlp; of_rlp; of_rlp_opt;
    marshal_rlp; unmarshal_rlp; unmarshal_rlp_opt;
    rlping ]

let rlp_op_map f { to_rlp_item; of_rlp_item; of_rlp_item_opt;
                   to_rlp; of_rlp; of_rlp_opt;
                   marshal_rlp; unmarshal_rlp; unmarshal_rlp_opt;
                   rlping }
  =
  { to_rlp_item = f to_rlp_item;
    of_rlp_item = f of_rlp_item;
    of_rlp_item_opt = f of_rlp_item_opt;
    to_rlp = f to_rlp;
    of_rlp = f of_rlp;
    of_rlp_opt = f of_rlp_opt;
    marshal_rlp = f marshal_rlp;
    unmarshal_rlp = f unmarshal_rlp;
    unmarshal_rlp_opt = f unmarshal_rlp_opt;
    rlping = f rlping }

let rlp_op_map3 f a b c =
  { to_rlp_item = f a.to_rlp_item b.to_rlp_item c.to_rlp_item;
    of_rlp_item = f a.of_rlp_item b.of_rlp_item c.of_rlp_item;
    of_rlp_item_opt = f a.of_rlp_item_opt b.of_rlp_item_opt c.of_rlp_item_opt;
    to_rlp = f a.to_rlp b.to_rlp c.to_rlp;
    of_rlp = f a.of_rlp b.of_rlp c.of_rlp;
    of_rlp_opt = f a.of_rlp_opt b.of_rlp_opt c.of_rlp_opt;
    marshal_rlp = f a.marshal_rlp b.marshal_rlp c.marshal_rlp;
    unmarshal_rlp = f a.unmarshal_rlp b.unmarshal_rlp c.unmarshal_rlp;
    unmarshal_rlp_opt = f a.unmarshal_rlp_opt b.unmarshal_rlp_opt c.unmarshal_rlp_opt;
    rlping = f a.rlping b.rlping c.rlping }

let rlp_op_names = { to_rlp_item = "to_rlp_item";
                     of_rlp_item = "of_rlp_item";
                     of_rlp_item_opt = "of_rlp_item_opt";
                     to_rlp = "to_rlp";
                     of_rlp = "of_rlp";
                     of_rlp_opt = "of_rlp_opt";
                     marshal_rlp = "marshal_rlp";
                     unmarshal_rlp = "unmarshal_rlp";
                     unmarshal_rlp_opt = "unmarshal_rlp_opt";
                     rlping = "rlping" }

(* These correspond to the operations each one "takes in" as an argument
   for converting a type parameter. *)
let rlp_op_directions = { to_rlp_item = "to_rlp_item";
                          of_rlp_item = "of_rlp_item";
                          of_rlp_item_opt = "of_rlp_item";
                          to_rlp = "to_rlp_item";
                          of_rlp = "of_rlp_item";
                          of_rlp_opt = "of_rlp_item";
                          marshal_rlp = "to_rlp_item";
                          unmarshal_rlp = "of_rlp_item";
                          unmarshal_rlp_opt = "of_rlp_item";
                          rlping = "rlping" }

(* ------------------------------------------------ *)

let exp_rlp_item str_exp    = Exp.construct (lid "Ppx_deriving_rlp_runtime_core.Rlp.RlpItem") (Some str_exp)
let exp_rlp_items lst_exp   = Exp.construct (lid "Ppx_deriving_rlp_runtime_core.Rlp.RlpItems") (Some lst_exp)
let exp_rlp_items_list exps = exp_rlp_items (list exps)

let pat_rlp_item str_pat    = Pat.construct (lid "Ppx_deriving_rlp_runtime_core.Rlp.RlpItem") (Some str_pat)
let pat_rlp_items lst_pat   = Pat.construct (lid "Ppx_deriving_rlp_runtime_core.Rlp.RlpItems") (Some lst_pat)
let pat_rlp_items_list pats = pat_rlp_items (plist pats)

let exp_rlp_construct name maybe_exp =
  match maybe_exp with
  | None     -> exp_rlp_item (str name)
  | Some exp -> exp_rlp_items_list [exp_rlp_item (str name); exp]

let pat_rlp_construct name maybe_pat =
  match maybe_pat with
  | None     -> pat_rlp_item (pstr name)
  | Some pat -> pat_rlp_items_list [pat_rlp_item (pstr name); pat]

(* ------------------------------------------------ *)

(* ppx_deriving_rlp always uses suffixes. Never prefixes, and never both. *)
let mangle_type_decl_suffix s = Ppx_deriving.mangle_type_decl (`Suffix s)
let mangle_lid_suffix s = Ppx_deriving.mangle_lid (`Suffix s)

let type_name_suffix suffix name =
  if name = "t" then suffix else (name ^ "_" ^ suffix)

let type_lid_suffix suffix { loc; txt } =
  match flatten txt with
  | [name] when List.mem name supported_pervasives_types ->
    { loc; txt = mangle_lid_suffix suffix (lid ("Ppx_deriving_rlp_runtime." ^ name)).txt}
  | [name; "t"] when List.mem name supported_module_t_types ->
    let name = String.uncapitalize_ascii name in
    { loc; txt = mangle_lid_suffix suffix (lid ("Ppx_deriving_rlp_runtime." ^ name)).txt}
  | _ ->
    { loc; txt = mangle_lid_suffix suffix txt}

let type_name_to_rlp = type_name_suffix "to_rlp_item"
let type_lid_to_rlp  = type_lid_suffix "to_rlp_item"
let type_name_of_rlp = type_name_suffix "of_rlp_item"
let type_lid_of_rlp  = type_lid_suffix "of_rlp_item"
let type_name_rlping = type_name_suffix "rlping"


(* funs_case : (funexp * funexp) -> string -> bidirectional_case *)
let funs_case (to_rlp, of_rlp) tmp =
  let data_pat = pvar tmp
  and rlp_pat  = pvar tmp
  and data_exp = app of_rlp [evar tmp]
  and rlp_exp  = app to_rlp [evar tmp] in
  (data_pat, rlp_pat, data_exp, rlp_exp)

(* Produces a tuple of the "to" function and the "of" function *)
let cases_to_funs bidi_cases name =
  ((lam (pvar "x")
     (Exp.match_
       (evar "x")
       (List.map case_data_to_rlp bidi_cases))),
   (lam (pvar "x")
     (Exp.match_
       ~attrs:[turn_off_warning_this_match_case_is_unused]
       (evar "x")
       ((List.map case_rlp_to_data bidi_cases)
        @
        [case_else_data_type_mismatch
          ("of_rlp_item: error parsing rlp data for type: " ^ name)]))))



let rec core_type_to_case typ tmp : bidirectional_case =
  let typ = Ppx_deriving.remove_pervasives ~deriver typ in
  match typ.ptyp_desc with
  | Ptyp_any                -> raise_errorf "cannot convert Ptyp_any to RLP, given: %s" (string_of_type typ)
  | Ptyp_var name           -> let tv_name = ("type_var_" ^ name)
                               in funs_case (evar (type_name_to_rlp tv_name), evar (type_name_of_rlp tv_name)) tmp
  | Ptyp_tuple tys          -> core_tuple_to_case tys tmp
  | Ptyp_constr (lid, [])   -> funs_case (Exp.ident (type_lid_to_rlp lid), Exp.ident (type_lid_of_rlp lid)) tmp
  | Ptyp_constr (lid, args) -> let (tos, ofs) = unzip2 (List.map core_type_to_funs args)
                               in funs_case (app (Exp.ident (type_lid_to_rlp lid)) tos, app (Exp.ident (type_lid_of_rlp lid)) ofs) tmp
  | Ptyp_alias (ty, _name)  -> core_type_to_case ty tmp
  | Ptyp_variant (fs, _, _) -> funs_case (poly_variants_to_funs fs typ (string_of_type typ)) tmp
  | Ptyp_arrow _            -> raise_errorf "cannot convert functions to RLP, given: %s" (string_of_type typ)
  | Ptyp_object _           -> raise_errorf "cannot convert objects to RLP (yet), given: %s" (string_of_type typ)
  | Ptyp_class _            -> raise_errorf "cannot convert classes to RLP (yet), given: %s" (string_of_type typ)
  | _                       -> raise_errorf "cannot convert type to RLP (yet), given: %s" (string_of_type typ)

and core_tuple_to_case tys tmp : bidirectional_case =
  let tmps = generate_temporaries tmp tys in
  let (data_pats, rlp_pats, data_exps, rlp_exps) = cases_unzip (List.map2 core_type_to_case tys tmps)
  in (Pat.tuple data_pats,
      pat_rlp_items_list rlp_pats,
      Exp.tuple data_exps,
      exp_rlp_items_list rlp_exps)

(* Produces a tuple of the "to" function and the "of" function *)
and core_type_to_funs typ =
  match typ.ptyp_desc with
  | Ptyp_any                -> raise_errorf "cannot convert Ptyp_any to RLP"
  | Ptyp_var name           -> let tv_name = ("type_var_" ^ name)
                               in (evar (type_name_to_rlp tv_name), evar (type_name_of_rlp tv_name))
  | Ptyp_constr (lid, [])   -> (Exp.ident (type_lid_to_rlp lid), Exp.ident (type_lid_of_rlp lid))
  | Ptyp_constr (lid, args) -> let (tos, ofs) = unzip2 (List.map core_type_to_funs args)
                               in (app (Exp.ident (type_lid_to_rlp lid)) tos, app (Exp.ident (type_lid_of_rlp lid)) ofs)
  | Ptyp_variant (fs, _, _) -> poly_variants_to_funs fs typ (string_of_type typ)
  | _                       -> let (data_pat, rlp_pat, data_exp, rlp_exp) = core_type_to_case typ "tmp"
                               in (lam data_pat rlp_exp,
                                   lam (pvar "x")
                                    (Exp.match_
                                      ~attrs:[turn_off_warning_this_match_case_is_unused]
                                      (evar "x")
                                      [Exp.case rlp_pat data_exp;
                                       case_else_data_type_mismatch
                                         ("of_rlp_item: error parsing rlp data for type: " ^ string_of_type typ)]))


(* Produces a tuple of the "to" function and the "of" function *)
and poly_variants_to_funs fields top_typ name =
  let is_rtag = function Rtag _ -> true | Rinherit _ -> false in
  let (rtags, rinherits) = List.partition is_rtag fields in
  let tmps = generate_temporaries "x" rtags in
  let rtag_cases = List.map2 poly_variant_rtag_to_case rtags tmps
  and rinherit_cases = poly_variant_rinherits_to_cases rinherits top_typ "sup" in
  cases_to_funs (rtag_cases @ rinherit_cases) name

and poly_variant_rtag_to_case field tmp : bidirectional_case =
  match field with
  | Rtag (label, _, true (*empty*), []) ->
    (Pat.variant label.txt None,
     pat_rlp_construct label.txt None,
     Exp.variant label.txt None,
     exp_rlp_construct label.txt None)
  | Rtag (label, _, false, [typ]) ->
    let (data_pat, rlp_pat, data_exp, rlp_exp) = core_type_to_case typ tmp in
    (Pat.variant label.txt (Some data_pat),
     pat_rlp_construct label.txt (Some rlp_pat),
     Exp.variant label.txt (Some data_exp),
     exp_rlp_construct label.txt (Some rlp_exp))
  | _ ->
    raise_errorf "cannot convert polymorphic variant to RLP (yet)"

and poly_variant_rinherits_to_cases rinherits top_typ tmp =
  match rinherits with
  | [] -> []
  | [Rinherit ({ ptyp_desc = Ptyp_constr (tname, _) } as typ)] ->
    let (data_pat, rlp_pat, data_exp, rlp_exp) = core_type_to_case typ tmp in
    [(Pat.alias (Pat.type_ tname) (mknoloc tmp),
      rlp_pat,
      Exp.coerce data_exp None top_typ,
      Exp.match_ (evar tmp) [Exp.case data_pat rlp_exp])]
  | _ ->
    (* TODO:
       This is possible in theory, it would just require
       generating a nested match expression such as this:
       match (sup1_of_rlp_item_opt x) with
       | Some y -> y
       | None ->
         (match (sup2_of_rlp_item_opt x) with
          | Some y -> y
          | None ->
            (match (sup3_of_rlp_item_opt x) with
             | Some y -> y
             | None ->
               raise Rlp_data_mismatch_error ...)) *)
    raise_errorf "cannot convert polymorphic variant with multiple inheritance"


let record_fields_to_case flds tmp =
  let tmps = generate_temporaries tmp flds
  and lids = List.map (fun fld -> str_to_lid fld.pld_name) flds
  and tys = List.map (fun fld -> fld.pld_type) flds in
  let (data_pats, rlp_pats, data_exps, rlp_exps) = cases_unzip (List.map2 core_type_to_case tys tmps)
  in (Pat.record (zip2 lids data_pats) Closed,
      ctor_tup_args pat_rlp_items_list rlp_pats,
      Exp.record (zip2 lids data_exps) None,
      ctor_tup_args exp_rlp_items_list rlp_exps)

let constructor_arguments_to_case pcd_args tmp =
  match pcd_args with
  | Pcstr_tuple tys ->
    let tmps = generate_temporaries tmp tys in
    let (data_pats, rlp_pats, data_exps, rlp_exps) = cases_unzip (List.map2 core_type_to_case tys tmps)
    in (ctor_tup_args Pat.tuple data_pats,
        ctor_tup_args pat_rlp_items_list rlp_pats,
        ctor_tup_args Exp.tuple data_exps,
        ctor_tup_args exp_rlp_items_list rlp_exps)
  | Pcstr_record flds ->
    let (data_pat, rlp_pat, data_exp, rlp_exp) = record_fields_to_case flds tmp
    in (Some data_pat, rlp_pat, Some data_exp, rlp_exp)

let type_decl_record_to_case (fields : label_declaration list) tmp =
  let (data_pat, rlp_pat, data_exp, rlp_exp) = record_fields_to_case fields tmp
  in (data_pat,
      (match rlp_pat with Some v -> v | None -> (pat_rlp_items_list [])),
      data_exp,
      (match rlp_exp with Some v -> v | None -> (exp_rlp_items_list [])))

let type_decl_variant_to_case i ({ pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }, tmp) =
  ignore pcd_loc; ignore pcd_attributes;
  (* pcd_res is used for GADTs, but we don't support those yet, so if its there, raise an error *)
  (match pcd_res with | None -> () | _ -> raise_errorf "ppx_deriving_rlp: GADTs are not yet supported");
  let i = Z.of_int i in
  let name_lid = str_to_lid pcd_name
  and name_tag = Ppx_deriving_rlp_runtime_core.Rlp_encode.encode_nat_as_string i
  and (data_pat, rlp_pat, data_exp, rlp_exp) = constructor_arguments_to_case pcd_args tmp
  in (Pat.construct name_lid data_pat,
      pat_rlp_construct name_tag rlp_pat,
      Exp.construct name_lid data_exp,
      exp_rlp_construct name_tag rlp_exp)

let type_decl_variants_to_cases (variants : constructor_declaration list) tmp =
  let tmps = generate_temporaries tmp variants
  in mapi2 type_decl_variant_to_case variants tmps

(* Produces a tuple of the "to" function and the "of" function *)
let type_decl_record_to_funs name fields =
  let bidi_case = type_decl_record_to_case fields "tmp" in
  cases_to_funs [bidi_case] name

(* Produces a tuple of the "to" function and the "of" function *)
let type_decl_variants_to_funs name variants =
  let bidi_cases = type_decl_variants_to_cases variants "tmp" in
  cases_to_funs bidi_cases name

(* Produces a tuple of the "to" function and the "of" function *)
let type_decl_manifest_to_funs manifest =
  match manifest with
  | None    -> raise_errorf "ppx_deriving_rlp: cannot find definition for abstract type"
  | Some ty ->
    let bidi_case = core_type_to_case ty "tmp" in
    cases_to_funs [bidi_case] (string_of_type ty)

(* Produces a tuple of the "to" function and the "of" function
  The rhs_typ_kind contains either the variants or the record fields
  for an Algebraic Data Type (ADT), such as:
    - `type foo = A of int | B of bool`
      the rhs_typ_kind is Ptype_variant with constructor declarations
      `A of int` and `B of bool`
    - `type foo = { s : string; v : int }`
      the rhs_typ_kind is Ptype_record with fields `s : string` and
      `v : int`
  The manifest is the thing on the right hand side of the `=` sign
   for a type alias such as:
     - `type foo = int`
       the manifest is Some with `int` *)
let type_decl_rhs_to_funs name rhs_typ_kind manifest =
  match rhs_typ_kind with
  | Ptype_abstract    -> type_decl_manifest_to_funs manifest
  | Ptype_variant vs  -> type_decl_variants_to_funs name vs
  | Ptype_record flds -> type_decl_record_to_funs name flds
  | Ptype_open        -> raise_errorf "ppx_deriving_rlp: open types are not yet supported"

(* handling type parameters *)
let type_decl_param_name (core_type, _variance) =
  match core_type.ptyp_desc with
  | Ptyp_var name -> "type_var_" ^ name
  | _             -> raise_errorf "ppx_deriving_rlp: expected type parameters to be names"

(* returns a tuple of three values:
    * pre : listof structure_item
    * vals : listof value_binding
    * post : listof structure_item
  *)
let str_of_type_derive_rlp ~options ~path type_decl =
  ignore options;
  ignore path;
  let catching exn = Exp.case (Pat.construct exn (Some (Pat.any ()))) exp_none in
  let name = rlp_op_map (fun s -> mangle_type_decl_suffix s type_decl) rlp_op_names
  and param_type_names = List.map type_decl_param_name type_decl.ptype_params in
  let param_to_rlp_names = List.map type_name_to_rlp param_type_names
  and param_of_rlp_names = List.map type_name_of_rlp param_type_names
  and param_rlping_names = List.map type_name_rlping param_type_names in
  let (to_fun, of_fun) = type_decl_rhs_to_funs type_decl.ptype_name.txt type_decl.ptype_kind type_decl.ptype_manifest in
  let rlp_op_exps = {
    to_rlp_item = to_fun;
    of_rlp_item = of_fun;
    of_rlp_item_opt = (lam (pvar "x")
                        (Exp.try_
                          (exp_some (app (evar name.of_rlp_item)
                                         (List.map evar (param_of_rlp_names @ ["x"]))))
                          [catching (lid "Ppx_deriving_rlp_runtime_core.Rlp.Rlp_data_type_mismatch")]));
    to_rlp = (lam (pvar "x")
               (app (Exp.ident (lid "Ppx_deriving_rlp_runtime_core.Rlp_encode.rlp_item_to_rlp"))
                    [app (evar name.to_rlp_item)
                         (List.map evar (param_to_rlp_names @ ["x"]))]));
    of_rlp = (lam (pvar "x")
               (app (evar name.of_rlp_item)
                    (List.map evar param_of_rlp_names
                     @
                     [(app (Exp.ident (lid "Ppx_deriving_rlp_runtime_core.Rlp_decode.rlp_item_of_rlp")) [evar "x"])])));
    of_rlp_opt = (lam (pvar "x")
                   (Exp.try_
                     (exp_some (app (evar name.of_rlp)
                                    (List.map evar (param_of_rlp_names @ ["x"]))))
                     (List.map catching [lid "Ppx_deriving_rlp_runtime_core.Rlp.Rlp_data_type_mismatch";
                                         lid "Ppx_deriving_rlp_runtime_core.Rlp.Rlp_unmarshaling_error"])));
    marshal_rlp = (lams (List.map pvar ["buffer"; "x"])
                    (app (Exp.ident (lid "Ppx_deriving_rlp_runtime_core.Rlp_encode.rlp_item_marshal_rlp"))
                         [evar "buffer";
                          app (evar name.to_rlp_item)
                              (List.map evar (param_to_rlp_names @ ["x"]))]));
    unmarshal_rlp = (lams (List.map pvar ["i"; "x"])
                      (app (Exp.ident (lid "Ppx_deriving_rlp_runtime_core.Private.unmarshal_of_of"))
                           [app (evar name.of_rlp_item)
                                (List.map evar param_of_rlp_names);
                            evar "i";
                            evar "x"]));
    unmarshal_rlp_opt = (lams (List.map pvar ["i"; "x"])
                          (Exp.try_
                            (exp_some (app (evar name.unmarshal_rlp)
                                           (List.map evar (param_of_rlp_names @ ["i"; "x"]))))
                            (List.map catching [lid "Ppx_deriving_rlp_runtime_core.Rlp.Rlp_data_type_mismatch";
                                                lid "Ppx_deriving_rlp_runtime_core.Rlp.Rlp_unmarshaling_error"])));
    rlping = let rlping_to_rlp param = Exp.field (evar param) (lid "Ppx_deriving_rlp_runtime_core.Rlping.to_rlp_item")
             and rlping_of_rlp param = Exp.field (evar param) (lid "Ppx_deriving_rlp_runtime_core.Rlping.of_rlp_item") in
             (Exp.record
              [(lid "Ppx_deriving_rlp_runtime_core.Rlping.to_rlp_item", app (evar name.to_rlp_item) (List.map rlping_to_rlp param_rlping_names));
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.of_rlp_item", app (evar name.of_rlp_item) (List.map rlping_of_rlp param_rlping_names));
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.of_rlp_item_opt", app (evar name.of_rlp_item_opt) (List.map rlping_of_rlp param_rlping_names));
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.to_rlp", app (evar name.to_rlp) (List.map rlping_to_rlp param_rlping_names));
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.of_rlp", app (evar name.of_rlp) (List.map rlping_of_rlp param_rlping_names));
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.of_rlp_opt", app (evar name.of_rlp_opt) (List.map rlping_of_rlp param_rlping_names));
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.marshal_rlp", app (evar name.marshal_rlp) (List.map rlping_to_rlp param_rlping_names));
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.unmarshal_rlp", app (evar name.unmarshal_rlp) (List.map rlping_of_rlp param_rlping_names));
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.unmarshal_rlp_opt", app (evar name.unmarshal_rlp_opt) (List.map rlping_of_rlp param_rlping_names))]
              None)
  }
  in

  let make_op_binding name dir exp =
    Vb.mk (pvar name)
          (lams (List.map pvar (List.map (type_name_suffix dir) param_type_names))
                exp)
  in
  let val_bindings = rlp_op_map3 make_op_binding name rlp_op_directions rlp_op_exps
  in
  ([], (rlp_ops_to_list val_bindings), [])

(* returns a tuple of three values:
    * pre : listof structure_item
    * vals : listof value_binding
    * post : listof structure_item
  *)
let str_of_type_rlp ~options ~path type_decl =
  match List.assoc_opt "rlping" options with
  | None -> str_of_type_derive_rlp ~options ~path type_decl
  | Some rlping_exp ->
    (match type_decl.ptype_params with
     | [] -> ()
     | (_::_) -> raise_errorf "{rlping = expression} attribute cannot be used with type parameters");
    let name = rlp_op_map (fun s -> mangle_type_decl_suffix s type_decl) rlp_op_names in
    let val_rlping = Vb.mk (pvar name.rlping) rlping_exp
    and val_rest =
      Vb.mk (Pat.record
              [(lid "Ppx_deriving_rlp_runtime_core.Rlping.to_rlp_item", pvar name.to_rlp_item);
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.of_rlp_item", pvar name.of_rlp_item);
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.of_rlp_item_opt", pvar name.of_rlp_item_opt);
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.to_rlp", pvar name.to_rlp);
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.of_rlp", pvar name.of_rlp);
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.of_rlp_opt", pvar name.of_rlp_opt);
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.marshal_rlp", pvar name.marshal_rlp);
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.unmarshal_rlp", pvar name.unmarshal_rlp);
               (lid "Ppx_deriving_rlp_runtime_core.Rlping.unmarshal_rlp_opt", pvar name.unmarshal_rlp_opt)]
              Closed)
            (evar name.rlping)
    in
    ([], [val_rlping], [Str.value Nonrecursive [val_rest]])

(* --------------------------------- *)

let rlping_expr_of_typ typ =
  let (to_exp, of_exp) = core_type_to_funs typ in
  (app
    (Exp.ident (lid "Ppx_deriving_rlp_runtime_core.Private.rlping_of_to_and_of"))
    [to_exp; of_exp])

(* --------------------------------- *)

let sig_of_type_rlp ~options ~path type_decl =
  ignore options;
  ignore path;
  let name = rlp_op_map (fun s -> mangle_type_decl_suffix s type_decl) rlp_op_names
  in

  let param_types = List.map (fun (t,_) -> t) type_decl.ptype_params in
  let type_defined = Typ.constr (str_to_lid type_decl.ptype_name) param_types
  and type_for_op op t = Typ.constr (lid ("Ppx_deriving_rlp_runtime_core.Rlping." ^ op)) [t] in
  let type_for = rlp_op_map type_for_op rlp_op_names
  and param_for = rlp_op_map type_for_op rlp_op_directions
  in

  let make_op_decl name param_for type_for =
    Sig.value (Val.mk (mknoloc name)
                      (arrows (List.map param_for param_types) (type_for type_defined)))
  in
  let op_decls = rlp_op_map3 make_op_decl name param_for type_for
  in

  (rlp_ops_to_list op_decls)

(* --------------------------------- *)

let () =
  Ppx_deriving.register
    (Ppx_deriving.create
      deriver
      ~core_type:rlping_expr_of_typ
      ~type_decl_str:(structure (on_str_decls str_of_type_rlp))
      (* ~type_ext_str:ser_str_of_type_ext *)
      ~type_decl_sig:(on_sig_decls sig_of_type_rlp)
      (* ~type_ext_sig:ser_sig_of_type_ext *)
      ())

