[%%metapackage metapp]

[%%meta
  let ocaml_minor_version =
    int_of_string (String.sub Sys.ocaml_version 2 2) in
  let make_converter field_name e =
    let rec convert minor_version e =
      if minor_version = 12 then
        e
      else
        let next_version =
          if minor_version < 12 then
            minor_version + 1
          else
            minor_version - 1 in
        let converter_name =
          Format.asprintf "Migrate_4%.2d_4%.2d"
            next_version minor_version in
        let converter =
          Metapp.Exp.ident
            (Ldot (Ldot (Lident "Astlib", converter_name),
              field_name)) in
        [%e [%meta converter] [%meta convert next_version e]] in
    convert ocaml_minor_version e in
  [%stri
    let copy_structure s = [%meta make_converter "copy_structure" [%e s]]
    and copy_signature s = [%meta make_converter "copy_signature" [%e s]]
    and copy_expression e = [%meta make_converter "copy_expression" [%e e]]
    and copy_pattern p = [%meta make_converter "copy_pattern" [%e p]]
    and copy_core_type t = [%meta make_converter "copy_core_type" [%e t]]]]

let add_dir d =
  let dir = Load_path.Dir.create d in
  Load_path.prepend_dir dir

let debug = ref false

(* BEGIN Stolen from Fl_dynload *)

let in_words s =
  (* splits s in words separated by commas and/or whitespace *)
  let l = String.length s in
  let rec split i j =
    if j < l then
      match s.[j] with
      | (' '|'\t'|'\n'|'\r'|',') ->
          if i<j then (String.sub s i (j-i)) :: (split (j+1) (j+1))
          else split (j+1) (j+1)
      | _ ->
          split i (j+1)
    else
      if i<j then [ String.sub s i (j-i) ] else []
  in
  split 0 0

let load_pkg pkg =
  if not (Findlib.is_recorded_package pkg) then (
     if !debug then
       Format.eprintf "[DEBUG] Fl_dynload: about to load: %s\n%!" pkg;
     (* Determine the package directory: *)
     let d = Findlib.package_directory pkg in
     add_dir d;
     (* First try the new "plugin" variable: *)
     let preds = Findlib.recorded_predicates() in
     let archive =
       try
         Findlib.package_property preds pkg "plugin"
       with
         | Not_found ->
              (* Legacy: use "archive" but require that the predicate
                 "plugin" is mentioned in the definition
               *)
              try
                let v, fpreds =
                  Findlib.package_property_2 ("plugin"::preds) pkg "archive" in
                let need_plugin =
                  List.mem "native" preds in
                if need_plugin && not (List.mem (`Pred "plugin") fpreds) then
                  ""
                else
                  v
              with Not_found -> "" in
     (* Split the plugin/archive property and resolve the files: *)
     let files = in_words archive in
     if !debug then
       Format.eprintf "[DEBUG] Fl_dynload: files=%S\n%!" archive;
     List.iter
       (fun file ->
          if !debug then
            Format.eprintf "[DEBUG] Fl_dynload: loading %S\n%!" file;
          let file = Findlib.resolve_path ~base:d file in
          Dynlink.loadfile file
       ) files;
     Findlib.record_package Findlib.Record_load pkg
  )
  else
    if !debug then
      Format.eprintf "[DEBUG] Fl_dynload: not loading: %s\n%!" pkg

let load_packages pkgs =
  let preds = Findlib.recorded_predicates() in
  let eff_pkglist =
    Findlib.package_deep_ancestors preds pkgs in
  List.iter load_pkg eff_pkglist

(* END Stolen from Fl_dynload *)

let require name =
  try
    load_packages [name];
  with Fl_package_base.No_such_package (name, _) ->
    raise (Py.Err (ImportError, Printf.sprintf "No such package: %s" name))

let python_of_longident longident =
  Format.asprintf "ocaml.%a" Printtyp.longident longident

(* Stolen from native/topeval.ml *)
module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol

  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol

  let size_int = Sys.word_size
  let big_endian = Sys.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end

let backend = (module Backend : Backend_intf.S)

let compile_and_load_structure ocaml_env module_name structure =
  if structure <> [] then
    begin
      let structure = copy_structure structure in
      if !debug then
        begin
          prerr_endline module_name;
          Format.eprintf "%a@." Pprintast.structure structure;
        end;
      Compilenv.reset module_name;
      let impl =
        Typemod.type_implementation "" "" module_name ocaml_env structure in
      let program =
        Translmod.transl_store_implementation module_name
          (impl.structure, impl.coercion) in
      let program =
        { program with code = Simplif.simplify_lambda program.code } in
      let filename_dll = Filename.temp_file "ocaml-in-python" Config.ext_dll in
      let prefixname = Filename.chop_extension filename_dll in
      let middle_end = Closure_middle_end.lambda_to_clambda in
      let ppf_dump = Format.err_formatter in
      Asmgen.compile_implementation ~backend ~prefixname ~middle_end ~ppf_dump
        program;
      let filename_cmx = prefixname ^ ".cmx" in
      Compilenv.save_unit_info filename_cmx;
      Asmlink.link_shared ~ppf_dump [filename_cmx] filename_dll;
      let filename_dll =
        if Filename.is_implicit filename_dll then
          Filename.concat (Sys.getcwd ()) filename_dll
        else
          filename_dll in
      Dynlink.loadfile filename_dll;
      (*Sys.remove filename_dll*)
    end

let current_module_index = ref None

let module_name index =
  Format.sprintf "Ocaml_in_python_dyn%d" index

let accu_structure = ref []

let push_structure structure =
  assert (!current_module_index <> None);
  accu_structure := List.rev_append structure !accu_structure

let preambles = ref []

let push_preamble structure =
  preambles := structure :: !preambles;
  push_structure structure

let pop_preample () =
  match !preambles with
  | _ :: tail -> preambles := tail
  | [] -> assert false

let count counter =
  let result = !counter in
  counter := succ result;
  result

let prepare_compilation_unsafe () =
  let index = count Ocaml_in_python_api.capsule_count in
  current_module_index := Some index;
  index

let prepare_compilation_opt () =
  match !current_module_index with
  | None -> Some (prepare_compilation_unsafe ())
  | Some _ -> None

let prepare_compilation_immediate () =
  assert (!current_module_index = None);
  prepare_compilation_unsafe ()

let root_ocaml_env = ref None

let perform_compilation () =
  let index = Option.get !current_module_index in
  let structure = List.rev !accu_structure in
  let module_name = module_name index in
  compile_and_load_structure (Option.get !root_ocaml_env) module_name structure;
  accu_structure := [];
  current_module_index := None

let cut_compilation () =
  if List.length !accu_structure >= 100 then
    begin
      perform_compilation ();
      ignore (prepare_compilation_unsafe ());
      List.iter push_structure (List.rev !preambles)
    end

let catch_compiler_errors f =
  try
    f ()
  with Env.Error error ->
         let error_msg = Format.asprintf "%a" Env.report_error error in
         raise (Py.Err (ImportError, error_msg))
     | exn ->
         match Location.error_of_exn exn with
         | None -> raise exn
         | Some (`Ok error) ->
             let error_msg = Format.asprintf "%a" Location.print_report error in
             raise (Py.Err (ImportError, error_msg))
         | Some `Already_displayed -> assert false
(*
     | exc ->
         raise (Py.Err (ImportError, Printexc.to_string exc))
*)

let make_python_tuple (values : Ppxlib.expression list) : Ppxlib.expression =
  let length = List.length values in
  if length = 0 then
    [%expr Py.Tuple.empty]
  else [%expr
    let result = Py.Tuple.create [%e Metapp.Exp.of_int length] in
    [%e Metapp.sequence (values |> List.mapi (fun i v ->
      [%expr Py.Tuple.set_item result [%e Metapp.Exp.of_int i] [%e v]]))];
    result]

let make_python_dict values : Ppxlib.expression = [%expr
  let result = Py.Dict.create () in
  [%e Metapp.sequence (values |> List.map (fun (key, optional, conv, value) ->
    if optional then [%expr
      match [%e value] with
      | None -> ()
      | Some value ->
          Py.Dict.set_item result [%e Metapp.Exp.of_string key]
            [%e conv [%expr value]]]
    else [%expr
      Py.Dict.set_item result [%e Metapp.Exp.of_string key] [%e conv value]]))];
    result]

let var_f i = Printf.sprintf "f%d" i

let make_python_sequence ?setitem classname ~getitem ~len =
  let methods = [
    [%expr "__getitem__", Py.Callable.of_function_as_tuple (fun tuple ->
      let (self, index) = Py.Tuple.to_tuple2 tuple in [%e
      getitem [%expr Py.Int.to_int index]])];
    [%expr "__len__", Py.Callable.of_function_as_tuple (fun _tuple ->
      Py.Int.of_int [%e len])]] in
  let methods =
    match setitem with
    | None -> methods
    | Some setitem ->
        [%expr "__setitem__", Py.Callable.of_function_as_tuple (fun tuple ->
          let (self, index, value) = Py.Tuple.to_tuple3 tuple in [%e
          setitem [%expr Py.Int.to_int index] [%expr value]];
          Py.none)]
        :: methods in [%expr
  let abc = Py.Import.import_module "collections.abc" in
  let sequence = Py.Module.get abc "Sequence" in
  Py.Class.init [%e Metapp.Exp.of_string classname] ~parents:[sequence]
    ~methods:[%e Metapp.Exp.list methods]]

module StringHashtbl = Hashtbl.MakeSeeded (struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.seeded_hash
end)

let convert_label (label : Asttypes.arg_label) : Ppxlib.arg_label =
  match label with
  | Nolabel -> Nolabel
  | Labelled label -> Labelled label
  | Optional label -> Optional label

let uid_of_type_path ocaml_env path =
  try
    (Ocaml_common.Env.find_type path ocaml_env).type_uid
  with Not_found ->
    failwith (Format.asprintf "Unbound type %a" Path.print path)

let uid_of_type_lident ocaml_env (lident : Longident.t) =
  let _path, td =
    Ocaml_common.Env.lookup_type ~loc:Location.none lident ocaml_env in
  td.type_uid

let import_ocaml_module_in_python_ref = ref (fun _ _ ->
  failwith "not available yet")

let import_ocaml_module_in_python ocaml_env (expansions : Path.t Path.Map.t) =
  !import_ocaml_module_in_python_ref ocaml_env expansions

module StringSet = Set.Make (String)

let is_unit_type (ty : Ocaml_in_python_api.Type.t) =
  match ty with
  | Constr (path, []) when Path.same path Predef.path_unit -> true
  | _ -> false

module VentilateParams = struct
  type t = {
      labels : string list;
      optional_labels : string list;
      no_label_count : int;
    }

  let empty = {
      labels = [];
      optional_labels = [];
      no_label_count = 0;
    }

  let add (accu : t) (param : Ocaml_in_python_api.Type.param) =
    match param.label with
    | Nolabel ->
        if is_unit_type param.ty then
          accu
        else
          { accu with no_label_count = succ accu.no_label_count }
    | Labelled label ->  { accu with labels = label :: accu.labels }
    | Optional label ->
        { accu with optional_labels = label :: accu.optional_labels }

  let ventilate (params : Ocaml_in_python_api.Type.param list) =
    let result = List.fold_left add empty params in
    { labels = List.rev result.labels;
      optional_labels = List.rev result.optional_labels;
      no_label_count = result.no_label_count; }
end

let check_arguments (arity : Ocaml_in_python_api.Type.arity) =
  let params = VentilateParams.ventilate arity.params in
  [%expr
    let nb_args = Py.Tuple.size args_tuple in
    if nb_args <> [%e Metapp.Exp.of_int params.no_label_count] then
      raise (Py.Err (RuntimeError,
        Printf.sprintf "%d positional argument%s expected but %d given"
          [%e Metapp.Exp.of_int params.no_label_count]
          [%e if params.no_label_count = 1 then Metapp.Exp.of_string ""
            else Metapp.Exp.of_string "s"]
          nb_args));
    [%e Metapp.sequence (params.labels |> List.map (fun label ->
       [%expr if keywords_dict = Py.null || Py.Dict.get_item_string keywords_dict [%e Metapp.Exp.of_string label] = None then
         raise (Py.Err (RuntimeError,
           Printf.sprintf "labelled argument '%s' expected"
              [%e Metapp.Exp.of_string label]))]))];
    if keywords_dict <> Py.null then
    keywords_dict |> Py.Dict.iter (fun key _value ->
      let key = Py.String.to_string key in
      if [%e List.fold_left
           (fun acc label ->
             [%expr [%e acc] && key <> [%e Metapp.Exp.of_string label]])
           [%expr key <> "type"]
           (params.labels @ params.optional_labels)] then
         raise (Py.Err (RuntimeError,
           Printf.sprintf "unknown labelled argument '%s'" key)))]

let type_constr_converter_tbl :
      (Env.t -> Path.t Path.Map.t -> Ocaml_in_python_api.Type.t list -> Ocaml_in_python_api.value_converter) Types.Uid.Tbl.t =
  Types.Uid.Tbl.create 16

let exn_converter_ref = ref (fun _ocaml_env _expansions : Ocaml_in_python_api.value_converter -> {
  python_of_ocaml = Implicit [%expr raise];
  ocaml_of_python = Explicit (fun _v -> [%expr failwith "Not available"]);
})

let make_python_function_call ocaml_env expansions
  (result_converter : Ocaml_in_python_api.value_converter) v python_args
  python_dict = [%expr
  try
    [%e Ocaml_in_python_api.Function.apply result_converter.ocaml_of_python
      [%expr Py.Callable.to_function_as_tuple_and_dict [%e v]
        [%e python_args] [%e python_dict]]]
  with (Py.E (_, obj)) as exc ->
    let exc' =
      try
        [%e Ocaml_in_python_api.Function.apply
          (!exn_converter_ref ocaml_env expansions).ocaml_of_python [%expr obj]]
      with _ ->
        raise exc in
    raise exc']

let make_python_string s =
  [%expr Py.String.of_string [%e Metapp.Exp.of_string s]]

let make_python_int s =
  [%expr Py.Int.of_int [%e Metapp.Exp.of_int s]]

let make_property ?(getter = [%expr Py.none]) ?(setter = [%expr Py.none]) () =
  [%expr
    Py.Module.get_function (Py.Module.builtins ()) "property"
      [%e Metapp.Exp.array [getter; setter]]]

let make_ocaml_function_call ocaml_env expansions arity (result_converter : Ocaml_in_python_api.value_converter) f ocaml_exps = [%expr
  [%e check_arguments arity];
  try
    let result = [%e Ppxlib.Ast_helper.Exp.apply f ocaml_exps] in
    [%e Ocaml_in_python_api.Function.apply result_converter.python_of_ocaml
      [%expr result]]
  with
  | (Py.E _ | Py.Err _) as exc -> raise exc
  | exc ->
      let obj =
        [%e Ocaml_in_python_api.Function.apply
          (!exn_converter_ref ocaml_env expansions).python_of_ocaml [%expr exc]] in
      let class_ = Py.Object.find_attr_string obj "__class__" in
      raise (Py.E (class_, obj))]

module Type = struct
  include Ocaml_in_python_api.Type

  let has_none ocaml_env (ty : t) =
    match ty with
    | Any
    | Var _ -> true
    | Arrow _ -> false
    | Tuple _ -> false
    | Constr (path, _args) ->
        let td = Ocaml_common.Env.find_type path ocaml_env in
        match td.type_kind with
        | Type_abstract -> false
        | Type_record _ -> false
        | Type_variant (constructors, _) ->
            constructors |> List.exists (fun
              (constructor : Types.constructor_declaration) ->
              Ident.name constructor.cd_id = "None")
        | Type_open -> true

  module Vars = struct
    type nonrec t = {
        names : string option Ocaml_in_python_api.ExtensibleArray.t;
        table : t Ocaml_in_python_api.IntHashtbl.t;
      }

    let count (vars : t) =
      Ocaml_in_python_api.ExtensibleArray.length vars.names

    let create () = {
        names = Ocaml_in_python_api.ExtensibleArray.create None 16;
        table = Ocaml_in_python_api.IntHashtbl.create 16;
      }

    let get_name (vars : t) (i : int) =
      Ocaml_in_python_api.ExtensibleArray.get vars.names i

    let fresh ?name ~id (vars : t) =
      let index = Ocaml_in_python_api.ExtensibleArray.push vars.names name in
      Ocaml_in_python_api.IntHashtbl.add vars.table id (Var index);
      index

    let bind (vars : t) (ty : Types.type_expr) ty' =
      Ocaml_in_python_api.IntHashtbl.add vars.table ty.id ty'

    let find (vars : t) (ty : Types.type_expr) =
      try
        Ocaml_in_python_api.IntHashtbl.find vars.table ty.id
      with Not_found ->
        let name =
          match ty.desc with
          | Tvar name -> name
          | _ -> assert false in
        let ty' = Var (fresh ?name ~id:ty.id vars) in
        Ocaml_in_python_api.IntHashtbl.add vars.table ty.id ty';
        ty'
  end

  let value_converter_of_tuple = ref (fun (_env : Env.t) (_ : Path.t Path.Map.t) (_args : t list) : Ocaml_in_python_api.value_converter ->
    failwith "not available yet")

  let expand_path expansions path =
    try
      Path.Map.find path expansions
    with Not_found ->
      path

  let rec of_type_expr (vars : Vars.t) (ocaml_env : Env.t) expansions (ty : Types.type_expr) : t =
    match ty.desc with
    | Tvar _ -> Vars.find vars ty
    | Tarrow (label, param, result, _) ->
        let param = of_type_expr vars ocaml_env expansions param in
        let result = of_type_expr vars ocaml_env expansions result in
        Arrow ({ label = convert_label label; ty = param }, result)
    | Ttuple args ->
        Tuple (List.map (of_type_expr vars ocaml_env expansions) args)
    | Tconstr (path, args, _) ->
        let args = List.map (of_type_expr vars ocaml_env expansions) args in
        begin match Env.find_type_expansion path ocaml_env with
        | params, body, _ ->
            let vars' = Vars.create () in
            List.iter2 (Vars.bind vars') params args;
            of_type_expr vars' ocaml_env expansions body
        | exception Not_found ->
            Constr (expand_path expansions path, args)
        end
    | _ ->
        failwith "Not implemented"

  let is_pyobject path =
    match Path.flatten path with
    | `Ok (ident, list) -> Ident.name ident = "Pytypes" && list = ["pyobject"]
    | `Contains_apply -> false

  let id : Ocaml_in_python_api.value_converter = {
      ocaml_of_python = Explicit Fun.id;
      python_of_ocaml = Explicit Fun.id }

  let to_value_converter_impl ?name ocaml_env expansions (ty : t) : Ocaml_in_python_api.value_converter =
    match ty with
    | Any -> id
    | Constr (path, []) when is_pyobject path -> id
    | Var _ -> assert false
    | Arrow _ ->
        value_converter_of_function ?name ocaml_env expansions (arity_of_type ty)
    | Tuple args ->
        !value_converter_of_tuple ocaml_env expansions args
    | Constr (path, args) ->
        let uid = uid_of_type_path ocaml_env path in
        let converter =
          try
            Types.Uid.Tbl.find type_constr_converter_tbl uid
          with Not_found ->
            match
              match path with
              | Pident _ -> None
              | _ ->
                  ignore (import_ocaml_module_in_python ocaml_env
                    expansions (Ident.name (Path.head path)));
                  try
                    Some (Types.Uid.Tbl.find type_constr_converter_tbl uid)
                  with Not_found ->
                    None
            with
            | None ->
                failwith
                  (Format.asprintf "No conversion for %a" Path.print path)
            | Some result -> result in
        let result = converter ocaml_env expansions args in
        result

  let converters_of_arity_impl ocaml_env expansions arity : Ocaml_in_python_api.converters_of_arity =
    let add_arg (index, python_args, python_dict)
      (param : param) :
      _ * ((Ppxlib.arg_label * Ppxlib.pattern)
      * (Ppxlib.arg_label * Ppxlib.expression)) =
      match param.label with
      | Nolabel ->
          let arg_converter = to_value_converter ocaml_env expansions param.ty in
          if is_unit_type param.ty then
            (index, python_args, python_dict),
            ((Nolabel, [%pat? ()]), (Nolabel, [%expr ()]))
          else
            let var = Printf.sprintf "x%d" index in
            let python_args =
              Ocaml_in_python_api.Function.apply arg_converter.python_of_ocaml
                (Metapp.Exp.var var) :: python_args in
            (index + 1, python_args, python_dict),
            ((Nolabel, Metapp.Pat.var var),
              (Nolabel, Ocaml_in_python_api.Function.apply arg_converter.ocaml_of_python
                [%expr Py.Tuple.get args_tuple [%e Metapp.Exp.of_int index]]))
      | Labelled label ->
          let arg_converter = to_value_converter ocaml_env expansions param.ty in
          let python_dict =
            (label, false, Ocaml_in_python_api.Function.apply arg_converter.python_of_ocaml,
              Metapp.Exp.var label) :: python_dict in
          (index, python_args, python_dict),
          ((Labelled label, Metapp.Pat.var label),
            (Labelled label,
              Ocaml_in_python_api.Function.apply arg_converter.ocaml_of_python
                [%expr Py.Dict.find_string keywords_dict
                  [%e Metapp.Exp.of_string label]]))
      | Optional label ->
          let arg_converter =
            match param.ty with
            | Constr (_option, [arg]) ->
                to_value_converter ocaml_env expansions arg
            | _ -> assert false in
          let python_dict =
            (label, true, Ocaml_in_python_api.Function.apply arg_converter.python_of_ocaml,
              Metapp.Exp.var label) :: python_dict in
          (index, python_args, python_dict),
          ((Optional label, Metapp.Pat.var label),
            (Optional label, [%expr Option.map
              [%e Ocaml_in_python_api.Function.to_expression arg_converter.ocaml_of_python]
                (if keywords_dict = Py.null then None
                 else
                 Py.Dict.find_string_opt keywords_dict
                   [%e Metapp.Exp.of_string label])])) in
    let (_, python_args, python_dict), ocaml_args =
      List.fold_left_map add_arg (0, [], []) arity.params in
    let python_args = make_python_tuple (List.rev python_args) in
    let python_dict = make_python_dict (List.rev python_dict) in
    let ocaml_pats, ocaml_exps = List.split ocaml_args in
    { python_args; python_dict; ocaml_pats; ocaml_exps }

  let value_converter_of_function_impl ?name ocaml_env expansions arity :
        Ocaml_in_python_api.value_converter =
    let ({ python_args; python_dict; ocaml_pats; ocaml_exps } : Ocaml_in_python_api.converters_of_arity) =
      converters_of_arity ocaml_env expansions arity in
    let result_converter = to_value_converter ocaml_env expansions arity.result in
    let ocaml_of_python : Ocaml_in_python_api.Function.t =
      let body v =
        make_python_function_call ocaml_env expansions result_converter
          v python_args python_dict in
      let func v =
        List.fold_left (fun body (label, pat) ->
          Ppxlib.Ast_helper.Exp.fun_ label None pat body) (body v)
          (List.rev ocaml_pats) in
      Explicit func in
    let python_of_ocaml : Ocaml_in_python_api.Function.t =
      let stub f =
        make_ocaml_function_call ocaml_env expansions arity result_converter f
          ocaml_exps in
      let f = [%expr Py.Callable.of_function_as_tuple_and_dict] in
      let f =
        match name with
        | None -> f
        | Some name ->
            let name_expr = Metapp.Exp.of_string name in
            [%expr [%e f] ~name:[%e name_expr]] in
      Explicit (fun v -> [%expr [%e f] (fun args_tuple keywords_dict -> [%e stub v])]) in
    { ocaml_of_python; python_of_ocaml }

  let () =
    to_value_converter_ref := to_value_converter_impl;
    converters_of_arity_ref := converters_of_arity_impl;
    value_converter_of_function_ref := value_converter_of_function_impl
end

let make_python_sequence_of_array python_of_ocaml ?ocaml_of_python classname
      array =
  make_python_sequence classname
    ~len:[%expr Array.length [%e array]]
    ~getitem:(fun index -> [%expr
      if 0 <= [%e index] && [%e index] < Array.length [%e array] then
        [%e Ocaml_in_python_api.Function.apply python_of_ocaml
          [%expr [%e array].([%e index])]]
      else
        Ocaml_in_python_api.raise_index_out_of_bounds ~index
          ~length:(Array.length [%e array])])
    ?setitem:(match ocaml_of_python with
    | None -> None
    | Some ocaml_of_python ->
        Some (fun index value -> [%expr
        if 0 <= [%e index] && [%e index] < Array.length [%e array] then
          [%e array].([%e index]) <-
            [%e Ocaml_in_python_api.Function.apply ocaml_of_python value]
        else
          Ocaml_in_python_api.raise_index_out_of_bounds ~index
            ~length:(Array.length [%e array])]))

let local_capsule_name index =
  Format.sprintf "capsule%d" index

let fresh_variable_index counter : Ocaml_in_python_api.variable_index =
  let local_index = count counter in
  { module_index = Option.get !current_module_index; local_index }

let fresh_capsule_index () =
  fresh_variable_index Ocaml_in_python_api.capsule_count

let get_variable_ident get_local_name (variable_index : Ocaml_in_python_api.variable_index) :
      Longident.t =
  let local = get_local_name variable_index.local_index in
  let current = Option.get !current_module_index in
  if variable_index.module_index = current then
    Lident local
  else
    Ldot (Lident (module_name variable_index.module_index), local)

type lident_or_variable_index =
  | LidentRef of Longident.t
  | VariableIndex of Ocaml_in_python_api.variable_index

let get_variable get_local_name (ident : lident_or_variable_index) =
  match ident with
  | LidentRef ident -> [%expr ![%e Metapp.Exp.ident ident]]
  | VariableIndex index -> Metapp.Exp.ident (get_variable_ident get_local_name index)

let capsule_ident variable_index =
  get_variable_ident local_capsule_name variable_index

let push_capsule_declaration var name ty =
  push_structure [%str
    let [%p Metapp.Pat.var var] :
      ([%t ty] -> Py.Object.t) * (Py.Object.t -> [%t ty]) =
      Py.Capsule.make [%e name]]

module LabelInfo = struct
  type 'a t = {
      name : string;
      declaration : Types.label_declaration;
      ty : 'a;
    }

  let of_declaration (declaration : Types.label_declaration) =
    { name = Ident.name declaration.ld_id;
      declaration;
      ty = declaration.ld_type }

  let map f info = { info with ty = f info.ty }
end

module Field = struct
  type t = {
      index : int;
      name : string;
      mutable_flag : Asttypes.mutable_flag;
    }

  let of_label index (label : _ LabelInfo.t) = {
      index;
      name = label.name;
      mutable_flag = label.declaration.ld_mutable;
    }

  let of_index index = {
      index;
      name = Printf.sprintf "f%d" index;
      mutable_flag = Immutable;
    }

  let to_field field =
    let index_exp = Metapp.Exp.of_int field.index in
    field.name, make_property ()
      ~getter:[%expr Py.Callable.of_function_as_tuple
        (fun tuple -> Py.Sequence.get (Py.Tuple.get tuple 0)
          [%e index_exp])]
      ?setter:(match field.mutable_flag with
        | Immutable -> None
        | Mutable -> Some [%expr Py.Callable.of_function_as_tuple
          (fun tuple -> Py.Sequence.set (Py.Tuple.get tuple 0)
            [%e index_exp] (Py.Tuple.get tuple 1);
            Py.none)])

   let to_field_name field =
     make_python_string field.name
end

let find_tuple_capsule ocaml_env expansions (types : Type.t list) :
      Ocaml_in_python_api.variable_index =
  try
    Ocaml_in_python_api.TypeList.Hashtbl.find
      Ocaml_in_python_api.tuple_capsules types
  with Not_found ->
    let core_type = Type.to_core_type (Type.Tuple types) in
    let converters = List.map (Type.to_value_converter ocaml_env expansions) types in
    let nb_converters = List.length converters in
    let type_indexes = List.map Ocaml_in_python_api.Type.to_index types in
    let capsule_index = fresh_capsule_index () in
    let capsule_name = local_capsule_name capsule_index.local_index in
    let capsule = Metapp.Exp.var capsule_name in
    let types_exp =
      Metapp.Exp.list (type_indexes |> List.map (fun type_index ->
        [%expr Ocaml_in_python_api.Type.of_index
          [%e Metapp.Exp.of_int type_index]])) in
    push_capsule_declaration capsule_name
      [%expr Format.asprintf "ocaml.tuple[%a]"
        Ocaml_in_python_api.Type.format
          (Ocaml_in_python_api.Type.Tuple [%e types_exp])] core_type;
    let structure = [%str
      let () =
        let api =
          Py.Callable.to_function_as_tuple
            (Py.Object.find_attr_string
               (Ocaml_in_python_api.get_root_python_module ()) "tuple")
            Py.Tuple.empty in
        Py.Object.set_attr_string api "make"
          (Py.Callable.of_function_as_tuple (fun tuple ->
            let len = Py.Tuple.get tuple 1 in
            let () =
              if len <> Py.none &&
                Py.Int.to_int len <> [%e Metapp.Exp.of_int nb_converters] then
                raise (Py.Err (RuntimeError, Printf.sprintf
                  [%e Metapp.Exp.of_string
                  (Printf.sprintf "Length set to %%d but tuple is of length %d"
                    nb_converters)] (Py.Int.to_int len))) in
            let template_item = Py.Tuple.get tuple 2 in
            let tuple = [%e Ppxlib.Ast_helper.Exp.tuple (converters |> List.map
              (fun (converter : Ocaml_in_python_api.value_converter) -> Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                 [%expr template_item]))] in
            fst [%e capsule] tuple));
        Py.Object.set_attr_string api "make_from_sequence"
          (Py.Callable.of_function_as_tuple (fun tuple ->
            let sequence = Py.Tuple.get tuple 0 in
            let tuple = [%e Ppxlib.Ast_helper.Exp.tuple (converters |> List.mapi
              (fun i (converter : Ocaml_in_python_api.value_converter) -> Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                 [%expr Py.Tuple.get sequence [%e Metapp.Exp.of_int i]]))] in
            fst [%e capsule] tuple));
        Py.Object.set_attr_string api "length"
          (Py.Callable.of_function_as_tuple (fun tuple ->
            [%e make_python_int nb_converters]));
        Py.Object.set_attr_string api "getitem"
          (Py.Callable.of_function_as_tuple (fun tuple ->
            let [%p Ppxlib.Ast_helper.Pat.tuple (List.init nb_converters
              (fun i -> Metapp.Pat.var (var_f i)))] =
              snd [%e capsule] (Py.Tuple.get tuple 0) in [%e
            Ppxlib.Ast_helper.Exp.match_
              [%expr Py.Int.to_int (Py.Tuple.get tuple 1)]
              ((converters |> List.mapi (fun i (converter : Ocaml_in_python_api.value_converter) ->
                 Ppxlib.Ast_helper.Exp.case (Metapp.Pat.of_int i)
                   (Ocaml_in_python_api.Function.apply converter.python_of_ocaml
                     (Metapp.Exp.var (var_f i))))) @
                [Ppxlib.Ast_helper.Exp.case [%pat? index] [%expr
                  Ocaml_in_python_api.raise_index_out_of_bounds ~index
                    ~length:[%e Metapp.Exp.of_int nb_converters]]])]));
        Ocaml_in_python_api.TypeList.Hashtbl.add
          Ocaml_in_python_api.tuple_api [%e types_exp] api] in
    push_structure structure;
    Ocaml_in_python_api.TypeList.Hashtbl.add
      Ocaml_in_python_api.tuple_capsules types capsule_index;
    capsule_index

let find_tuple_api ocaml_env expansions (types : Type.t list) : Py.Object.t =
  try
    Ocaml_in_python_api.TypeList.Hashtbl.find Ocaml_in_python_api.tuple_api
      types
  with Not_found ->
    ignore (find_tuple_capsule ocaml_env expansions types);
    Ocaml_in_python_api.TypeList.Hashtbl.find Ocaml_in_python_api.tuple_api
      types

let make_bytes_api _ocaml_env _expansions : Py.Object.t =
  Py.Class.init "bytes" ~methods:[
    "make", Py.Callable.of_function_as_tuple (fun tuple ->
      let len = Py.Tuple.get tuple 1 in
      let template_item = Py.Tuple.get tuple 2 in
      fst Ocaml_in_python_api.bytes_capsule (Bytes.make (Py.Int.to_int len)
        (Ocaml_in_python_api.char_of_py template_item)));
    "make_from_sequence", Py.Callable.of_function_as_tuple (fun tuple ->
      let sequence = Py.Tuple.get tuple 0 in
      let len = Py.Sequence.length sequence in
      let s =
        if len = 1 then
          Bytes.of_string (Py.String.to_string (Py.Sequence.get sequence 0))
        else
          Bytes.init len (fun i ->
            Ocaml_in_python_api.char_of_py (Py.Sequence.get sequence i)) in
      fst Ocaml_in_python_api.bytes_capsule s);
    "length", Py.Callable.of_function_as_tuple (fun tuple ->
      let capsule = Py.Tuple.get tuple 0 in
      Py.Int.of_int (Bytes.length (snd Ocaml_in_python_api.bytes_capsule capsule)));
    "getitem", Py.Callable.of_function_as_tuple (fun tuple ->
      let capsule = Py.Tuple.get tuple 0 in
      let index = Py.Tuple.get tuple 1 in
      Ocaml_in_python_api.py_of_char
        (Bytes.get (snd Ocaml_in_python_api.bytes_capsule capsule) (Py.Int.to_int index)));
    "setitem", Py.Callable.of_function_as_tuple (fun tuple ->
      let capsule = Py.Tuple.get tuple 0 in
      let index = Py.Tuple.get tuple 1 in
      let char = Py.Tuple.get tuple 2 in
      Bytes.set (snd Ocaml_in_python_api.bytes_capsule capsule) (Py.Int.to_int index)
        (Ocaml_in_python_api.char_of_py char);
      Py.none);
    "to_string", Py.Callable.of_function_as_tuple (fun tuple ->
      let capsule = Py.Tuple.get tuple 0 in
      Py.String.of_string (Bytes.to_string
        (snd Ocaml_in_python_api.bytes_capsule capsule)));]

let value_converter_of_tuple ocaml_env expansions (types : Type.t list) : Ocaml_in_python_api.value_converter =
  let arg_converters = List.map (Type.to_value_converter ocaml_env expansions) types in
  let capsule_index = find_tuple_capsule ocaml_env expansions types in
  let type_indexes = List.map Ocaml_in_python_api.Type.to_index types in
  let capsule = capsule_ident capsule_index in
  let types =
    Metapp.Exp.list (type_indexes |> List.map (fun type_index ->
      [%expr Ocaml_in_python_api.Type.of_index
        [%e Metapp.Exp.of_int type_index]])) in
  let ocaml_of_python : Ocaml_in_python_api.Function.t =
    Explicit (fun v -> [%expr
      if Py.Object.is_instance [%e v] (Py.Object.find_attr_string
        (Ocaml_in_python_api.get_root_python_module ()) "tuple") then
        begin
          let capsule = Py.Object.find_attr_string [%e v] "_capsule" in
          let capsule =
            if capsule = Py.none then
              let api =
                Ocaml_in_python_api.TypeList.Hashtbl.find
                  Ocaml_in_python_api.tuple_api [%e types] in
              Py.Callable.to_function_as_tuple
                (Py.Object.find_attr_string [%e v] "_set_api")
                [%e make_python_tuple [v; [%expr api]]]
            else
              capsule in
          snd [%e Metapp.Exp.ident capsule] capsule
        end
      else
        [%e Ppxlib.Ast_helper.Exp.tuple (arg_converters |> List.mapi
          (fun i (converter : Ocaml_in_python_api.value_converter) -> Ocaml_in_python_api.Function.apply converter.ocaml_of_python
             [%expr Py.Sequence.get [%e v] [%e Metapp.Exp.of_int i]]))]]) in
  let python_of_ocaml : Ocaml_in_python_api.Function.t =
    Explicit (fun v -> [%expr
      let capsule = fst [%e Metapp.Exp.ident capsule] [%e v] in
      let api =
        Ocaml_in_python_api.TypeList.Hashtbl.find
          Ocaml_in_python_api.tuple_api [%e types] in
      Py.Callable.to_function_as_tuple_and_dict
        (Py.Object.find_attr_string
          (Ocaml_in_python_api.get_root_python_module ()) "tuple")
        Py.Tuple.empty
        (Py.Dict.of_bindings_string ["__capsule", capsule; "api", api])]) in
  { ocaml_of_python; python_of_ocaml }

type collection_api = {
    classname : string;
    api_classname : string;
    capsules_table : Ocaml_in_python_api.variable_index Type.Hashtbl.t;
    capsules_table_name : string;
    api_table : Py.Object.t Type.Hashtbl.t;
    api_table_name : string;
    setitem : bool;
  }

let list_api = {
    classname = "ocaml.list";
    api_classname = "__list_api";
    capsules_table = Ocaml_in_python_api.list_capsules;
    capsules_table_name = "list_capsules";
    api_table = Ocaml_in_python_api.list_api;
    api_table_name = "list_api";
    setitem = false;
  }

let array_api = {
    classname = "ocaml.array";
    api_classname = "__array_api";
    capsules_table = Ocaml_in_python_api.array_capsules;
    capsules_table_name = "array_capsules";
    api_table = Ocaml_in_python_api.array_api;
    api_table_name = "array_api";
    setitem = true;
  }

let find_collection_capsule collection_api ocaml_env expansions (ty : Type.t) :
      Ocaml_in_python_api.variable_index =
  try
    Type.Hashtbl.find collection_api.capsules_table ty
  with Not_found ->
    let core_type = Type.to_core_type ty in
    let converter = Type.to_value_converter ocaml_env expansions ty in
    let type_index = Ocaml_in_python_api.Type.to_index ty in
    let capsule_index = fresh_capsule_index () in
    let capsule_name = local_capsule_name capsule_index.local_index in
    let capsule = Metapp.Exp.var capsule_name in
    push_capsule_declaration capsule_name [%expr
      Format.asprintf [%e Metapp.Exp.of_string (
          Printf.sprintf "%s[%%a]" collection_api.classname)]
        Ocaml_in_python_api.Type.format
          (Ocaml_in_python_api.Type.of_index
            [%e Metapp.Exp.of_int type_index])] [%type: [%t core_type] array];
    let structure = [%str
      let () =
        let api =
          Py.Callable.to_function_as_tuple
            (Py.Object.find_attr_string
               (Ocaml_in_python_api.get_root_python_module ())
                 [%e Metapp.Exp.of_string collection_api.api_classname])
            Py.Tuple.empty in
        Py.Object.set_attr_string api "make"
          (Py.Callable.of_function_as_tuple (fun tuple ->
            let len = Py.Tuple.get tuple 1 in
            let len =
              if len = Py.none then
                1
              else
                Py.Int.to_int len in
            let template_item = [%e
              Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                [%expr Py.Tuple.get tuple 2]] in
            fst [%e capsule] (Array.make len template_item)));
        Py.Object.set_attr_string api "make_from_sequence"
          (Py.Callable.of_function_as_tuple (fun tuple ->
            let sequence = Py.Tuple.get tuple 0 in
            fst [%e capsule]
              (Py.Sequence.to_array_map [%e Ocaml_in_python_api.Function.to_expression
              converter.ocaml_of_python] sequence)));
        Py.Object.set_attr_string api "length"
          (Py.Callable.of_function_as_tuple (fun tuple ->
            let capsule = Py.Tuple.get tuple 0 in
            assert (capsule != Py.null);
            Py.Int.of_int (Array.length (snd [%e capsule] capsule))));
        Py.Object.set_attr_string api "getitem"
          (Py.Callable.of_function_as_tuple (fun tuple ->
            let array = snd [%e capsule] (Py.Tuple.get tuple 0) in
            let index = Py.Int.to_int (Py.Tuple.get tuple 1) in
            let value =
              try array.(index)
              with Invalid_argument _ ->
                Ocaml_in_python_api.raise_index_out_of_bounds
                  ~index ~length:(Array.length array) in
            [%e Ocaml_in_python_api.Function.apply converter.python_of_ocaml [%expr value]]));
        [%e if collection_api.setitem then [%expr
          Py.Object.set_attr_string api "setitem"
            (Py.Callable.of_function_as_tuple (fun tuple ->
              let array = snd [%e capsule] (Py.Tuple.get tuple 0) in
              let index = Py.Int.to_int (Py.Tuple.get tuple 1) in
              let value =
                [%e Ocaml_in_python_api.Function.apply converter.ocaml_of_python [%expr
                   Py.Tuple.get tuple 2]] in
              begin try
                array.(index) <- value;
              with Invalid_argument _ ->
                Ocaml_in_python_api.raise_index_out_of_bounds
                  ~index ~length:(Array.length array)
              end;
              Py.none))]
        else [%expr ()]];
        Ocaml_in_python_api.Type.Hashtbl.add
          [%e Metapp.Exp.ident (Ldot (Lident "Ocaml_in_python_api",
            collection_api.api_table_name))]
          (Ocaml_in_python_api.Type.of_index [%e Metapp.Exp.of_int type_index])
          api] in
    push_structure structure;
    Type.Hashtbl.add collection_api.capsules_table ty capsule_index;
    capsule_index

let find_collection_api collection_api ocaml_env expansions (ty : Type.t) : Py.Object.t =
  try
    Type.Hashtbl.find collection_api.api_table ty
  with Not_found ->
    ignore (find_collection_capsule collection_api ocaml_env expansions ty);
    Type.Hashtbl.find collection_api.api_table ty

let value_converter_of_array ocaml_env expansions (arg : Type.t) : Ocaml_in_python_api.value_converter =
  let arg_converter = Type.to_value_converter ocaml_env expansions arg in
  let capsule_index = find_collection_capsule array_api ocaml_env expansions arg in
  let type_index = Ocaml_in_python_api.Type.to_index arg in
  let capsule = capsule_ident capsule_index in
  let ocaml_of_python : Ocaml_in_python_api.Function.t =
    Explicit (fun v -> [%expr
      if Py.Object.is_instance [%e v] (Py.Object.find_attr_string
        (Ocaml_in_python_api.get_root_python_module ()) "array") then
        begin
          let capsule = Py.Object.find_attr_string [%e v] "_capsule" in
          let capsule =
            if capsule = Py.none then
              let api =
                Ocaml_in_python_api.Type.Hashtbl.find
                  Ocaml_in_python_api.array_api
                  (Ocaml_in_python_api.Type.of_index
                    [%e Metapp.Exp.of_int type_index]) in
              Py.Callable.to_function_as_tuple
                (Py.Object.find_attr_string [%e v] "_set_api")
                [%e make_python_tuple [v; [%expr api]]]
            else
              capsule in
          snd [%e Metapp.Exp.ident capsule] capsule
        end
      else
        Py.Sequence.to_array_map
          [%e Ocaml_in_python_api.Function.to_expression arg_converter.ocaml_of_python] [%e v]]) in
  let python_of_ocaml : Ocaml_in_python_api.Function.t =
    Explicit (fun v -> [%expr
      let array = [%e v] in
      let capsule = fst [%e Metapp.Exp.ident capsule] array in
      let api =
        Ocaml_in_python_api.Type.Hashtbl.find
          Ocaml_in_python_api.array_api
          (Ocaml_in_python_api.Type.of_index
            [%e Metapp.Exp.of_int type_index]) in
      let result = Py.Callable.to_function_as_tuple_and_dict
        (Py.Object.find_attr_string
          (Ocaml_in_python_api.get_root_python_module ()) "array")
        Py.Tuple.empty
        (Py.Dict.of_bindings_string ["__capsule", capsule; "api", api]) in
      Py.Object.set_attr_string result "__match_args__"
        (Py.List.init (Array.length array) (fun i ->
          Py.String.of_string (Format.sprintf "f%d" i)));
      result ]) in
  { ocaml_of_python; python_of_ocaml }

let value_converter_of_list ocaml_env expansions (arg : Type.t) : Ocaml_in_python_api.value_converter =
  let arg_converter = Type.to_value_converter ocaml_env expansions arg in
  let capsule_index = find_collection_capsule list_api ocaml_env expansions arg in
  let type_index = Ocaml_in_python_api.Type.to_index arg in
  let capsule = capsule_ident capsule_index in
  let ocaml_of_python : Ocaml_in_python_api.Function.t =
    Implicit
      [%expr Py.Sequence.to_list_map
        [%e Ocaml_in_python_api.Function.to_expression arg_converter.ocaml_of_python]] in
  let python_of_ocaml : Ocaml_in_python_api.Function.t =
    Explicit (fun v -> [%expr
      let array = Array.of_list [%e v] in
      let capsule = fst [%e Metapp.Exp.ident capsule ] array in
      let api =
        Ocaml_in_python_api.Type.Hashtbl.find
          Ocaml_in_python_api.list_api
          (Ocaml_in_python_api.Type.of_index
            [%e Metapp.Exp.of_int type_index]) in
      let result = Py.Callable.to_function_as_tuple_and_dict
        (Py.Object.find_attr_string
          (Ocaml_in_python_api.get_root_python_module ()) "list")
        Py.Tuple.empty
        (Py.Dict.of_bindings_string ["__capsule", capsule; "api", api]) in
      Py.Object.set_attr_string result "__match_args__"
        (Py.List.init (Array.length array) (fun i ->
          Py.String.of_string (Format.sprintf "f%d" i)));
      result]) in
  { ocaml_of_python; python_of_ocaml }

let rec chop_other_type_declarations accu (list : Types.signature_item list) =
  match list with
  | Sig_type (ident, type_declaration, Trec_next, _visibility) :: tail ->
      chop_other_type_declarations ((ident, type_declaration) :: accu) tail
  | _ -> List.rev accu, list

let class_count = ref 0

module ConstructorArgs = struct
  type 'a t =
    | Tuple of 'a list
    | Record of 'a LabelInfo.t list

  let of_constructor_arguments (args : Types.constructor_arguments) =
    match args with
    | Cstr_tuple list -> Tuple list
    | Cstr_record labels ->
        Record (List.map LabelInfo.of_declaration labels)

  let length args =
    match args with
    | Tuple list -> List.length list
    | Record labels -> List.length labels

  let map f args =
    match args with
    | Tuple list -> Tuple (List.map f list)
    | Record labels -> Record (List.map (LabelInfo.map f) labels)

  let to_list_mapi f args =
    match args with
    | Tuple list -> List.mapi f list
    | Record labels -> List.mapi (fun i (info : _ LabelInfo.t) -> f i info.ty) labels
end

module Constructor = struct
  type ('ty, 'name) t = {
      name : 'name;
      class_var : Ocaml_in_python_api.variable_index;
      args : 'ty ConstructorArgs.t;
      result : 'ty option
    }

  let map f cstr = { cstr with
    args = ConstructorArgs.map f cstr.args; result = Option.map f cstr.result }

  let of_constructor_declaration (cstr : Types.constructor_declaration) =
    { name = Ident.name cstr.cd_id;
      class_var = fresh_variable_index class_count;
      args = ConstructorArgs.of_constructor_arguments cstr.cd_args;
      result = cstr.cd_res;
    }

  let of_extension_constructor (name : Longident.t) (cstr : Types.extension_constructor) =
    { name;
      class_var = fresh_variable_index class_count;
      args = ConstructorArgs.of_constructor_arguments cstr.ext_args;
      result = cstr.ext_ret_type;
    }
end

type open_type_constructors =
  (Type.t, Longident.t) Constructor.t option Ocaml_in_python_api.ExtensibleArray.t

type 'a type_kind =
  | Abstract
  | Record of 'a LabelInfo.t list
  | Variant of ('a, string) Constructor.t list
  | Open of open_type_constructors

type type_info = {
    index : int;
    name : string;
    longident : Longident.t;
    class_var : lident_or_variable_index;
    mutable capsule_var : Ocaml_in_python_api.TypeList.t -> Ocaml_in_python_api.variable_index;
    kind : Types.type_expr type_kind;
    type_declaration : Types.type_declaration;
  }

type open_type = {
    index : int;
    name : string;
    constructors : open_type_constructors;
    class_var : lident_or_variable_index;
  }

let open_types_tbl : open_type Types.Uid.Tbl.t = Types.Uid.Tbl.create 16

let type_count = ref 0

let get_local_class_var class_index =
  Format.asprintf "class%d" class_index

let make_class parents fields classname = [%expr
  Py.Class.init ~parents:[%e Metapp.Exp.list parents]
      ~fields:[%e Metapp.Exp.list (fields |>
        List.map (fun (field_name, value) ->
          [%expr ([%e Metapp.Exp.of_string field_name], [%e value])]))]
      [%e Metapp.Exp.of_string classname]]

let get_module_name (ident : Longident.t) =
  match ident with
  | Ldot (module_name, _str) ->
      Format.asprintf "%a" Pprintast.longident module_name
  | _ ->
      "<unknown>"

let push_constructor_class longident classname class_var_exp get_cstr_name i (cstr : _ Constructor.t) =
  let cstr_name = get_cstr_name cstr.name in
  let classname = Printf.sprintf "%s.%s" classname cstr_name in
  let length = ConstructorArgs.length cstr.args in
  let field_list =
    match cstr.args with
    | Tuple args ->
        List.mapi (fun i _ -> Field.of_index i) args
    | Record labels ->
        List.mapi Field.of_label labels in
  let field_names =
    make_python_tuple (field_list |> List.map Field.to_field_name) in
  let fields =
    ("__module__", make_python_string (get_module_name longident)) ::
    ("_constructor_name", make_python_string cstr_name) ::
    ("_constructor_index", make_python_int i) ::
    ("_default_length", make_python_int length) ::
    ("_field_names",
      match cstr.args with
      | Tuple _ -> [%expr Py.none]
      | Record _ -> field_names) ::
    ("__match_args__", field_names) ::
    List.map Field.to_field field_list in
  push_structure [%str
    let [%p Metapp.Pat.var (get_local_class_var cstr.class_var.local_index)] = [%e
      make_class [class_var_exp] fields classname]
    let () =
      Py.Object.set_attr_string [%e class_var_exp]
        [%e Metapp.Exp.of_string cstr_name]
        [%e Metapp.Exp.var (get_local_class_var cstr.class_var.local_index)]]

let variant_class = [%expr
  Py.Module.get (Ocaml_in_python_api.get_root_python_module ())
    "variant"]

let add_class_prototype (longident : Longident.t)
      ((ident : Ident.t), (type_declaration : Types.type_declaration)) =
  let index = count type_count in
  let name = Ident.name ident in
  let longident = Longident.Ldot (longident, name) in
  let classname =
    Format.asprintf "ocaml.%a" Pprintast.longident longident in
  let class_var = fresh_variable_index class_count in
  let nb_params = List.length type_declaration.type_params in
  let monomorphic = nb_params = 0 in
  let capsule_var _ = failwith "Not yet available capsule_var" in
  let kind =
    match type_declaration.type_kind with
    | Type_abstract ->
        let abstract_class = [%expr
          Py.Module.get (Ocaml_in_python_api.get_root_python_module ())
            "abstract"] in
        push_structure [%str let [%p Metapp.Pat.var (get_local_class_var class_var.local_index)] =
          [%e make_class [abstract_class] [] classname]];
        Abstract
    | Type_record (labels, _) ->
        let labels = List.map LabelInfo.of_declaration labels in
        let record_class = [%expr
          Py.Module.get (Ocaml_in_python_api.get_root_python_module ())
            "record"] in
        let field_list = List.mapi Field.of_label labels in
        let field_names =
          make_python_tuple (field_list |> List.map Field.to_field_name) in
        let fields =
          ("__module__", make_python_string (get_module_name longident)) ::
          ("_default_length", make_python_int (List.length labels)) ::
          ("_api_for_type", [%expr Py.none]) ::
          ("_field_names", field_names) ::
          ("__match_args__", field_names) ::
          List.map Field.to_field field_list in
        let fields =
          if monomorphic then
            ("_default_type", [%expr Py.Tuple.empty ]) :: fields
          else
            fields in
        push_structure
          [%str let [%p Metapp.Pat.var (get_local_class_var class_var.local_index)] =
            [%e make_class [record_class] fields classname]];
        Record labels
    | Type_variant (constructors, _) ->
        let constructors =
          List.map Constructor.of_constructor_declaration constructors in
        let fields =
          ("__module__", make_python_string (get_module_name longident)) ::
          (constructors |>
          List.map (fun (cstr : _ Constructor.t) ->
            cstr.name, [%expr Py.none])) in
        push_structure [%str let [%p Metapp.Pat.var (get_local_class_var class_var.local_index)] =
          [%e make_class [variant_class] fields classname]];
        let class_var_exp = Metapp.Exp.var (get_local_class_var class_var.local_index) in
        List.iteri (push_constructor_class longident classname class_var_exp Fun.id) constructors;
        Variant constructors
    | Type_open ->
        push_structure [%str let [%p Metapp.Pat.var (get_local_class_var class_var.local_index)] =
          [%e make_class [variant_class] [] classname]];
        let constructors =
          Ocaml_in_python_api.ExtensibleArray.create None 16 in
        Types.Uid.Tbl.add open_types_tbl type_declaration.type_uid {
          constructors; name; class_var = VariableIndex class_var; index };
        Open constructors in
  { index; name; longident; class_var = VariableIndex class_var; capsule_var; kind; type_declaration }

let get_local_capsule_var index =
  Format.asprintf "capsule%d" index

let converter_cache = Ocaml_in_python_api.IntHashtbl.create 16

let converter_counter = ref 0

let python_of_ocaml index =
  Printf.sprintf "python_of_ocaml%d" index

let ocaml_of_python index =
  Printf.sprintf "ocaml_of_python%d" index

let make_type_converter (type_info : type_info) ocaml_env expansions params :
  Ocaml_in_python_api.value_converter =
  let params_indexes =
    List.map Ocaml_in_python_api.Type.to_index params in
  let capsule_var = type_info.capsule_var params in
  let capsule_var =
    Metapp.Exp.ident (get_variable_ident get_local_capsule_var capsule_var) in
  let table =
    try
      Ocaml_in_python_api.IntHashtbl.find converter_cache type_info.index
    with Not_found ->
      let table = Ocaml_in_python_api.TypeList.Hashtbl.create 16 in
      Ocaml_in_python_api.IntHashtbl.add converter_cache type_info.index
        table;
      table in
  let converter_index =
    try
      Ocaml_in_python_api.TypeList.Hashtbl.find table params
    with Not_found ->
      let index = fresh_variable_index converter_counter in
      Ocaml_in_python_api.TypeList.Hashtbl.add table params index;
      let class_var = get_variable get_local_class_var type_info.class_var in
      push_structure [%str
        let rec [%p Metapp.Pat.var (python_of_ocaml index.local_index)] = fun v ->
          let make =
            [%e match type_info.kind with
            | Abstract -> [%expr fun capsule ->
                Py.Callable.to_function_as_tuple_and_dict
                  [%e class_var]
                  Py.Tuple.empty (Py.Dict.singleton_string "__capsule" capsule)]
            | Record _ -> [%expr
                let type_def_info =
                  Ocaml_in_python_api.IntHashtbl.find
                    Ocaml_in_python_api.type_def_table
                    [%e Metapp.Exp.of_int type_info.index] in
                let types =
                  List.map Ocaml_in_python_api.Type.of_index
                    [%e Metapp.Exp.list
                      (List.map Metapp.Exp.of_int params_indexes)] in
                type_def_info.make_api types;
                (Ocaml_in_python_api.TypeList.Hashtbl.find
                  type_def_info.api_table types).make]
            | Variant _ -> [%expr
                let type_def_info =
                  Ocaml_in_python_api.IntHashtbl.find
                    Ocaml_in_python_api.variant_table
                    [%e Metapp.Exp.of_int type_info.index] in
                let types =
                  List.map Ocaml_in_python_api.Type.of_index
                    [%e Metapp.Exp.list
                      (List.map Metapp.Exp.of_int params_indexes)] in
                type_def_info.make_api types;
                (Ocaml_in_python_api.TypeList.Hashtbl.find
                  type_def_info.api_table types).make]
            | Open _ -> [%expr
                let type_def_info =
                  Ocaml_in_python_api.IntHashtbl.find
                    Ocaml_in_python_api.OpenType.table
                    [%e Metapp.Exp.of_int type_info.index] in
                let types =
                  List.map Ocaml_in_python_api.Type.of_index
                    [%e Metapp.Exp.list
                      (List.map Metapp.Exp.of_int params_indexes)] in
                type_def_info.make_api types;
                (Ocaml_in_python_api.TypeList.Hashtbl.find
                  type_def_info.api_table types).make]] in
          make (fst [%e capsule_var] v)
        and [%p Metapp.Pat.var (ocaml_of_python index.local_index)] = fun v ->
          if Py.Object.is_instance v
            [%e class_var] then
              let capsule = Py.Object.find_attr_string v "_capsule" in
              let capsule =
                if capsule = Py.none then
                  [%e match type_info.kind with
                      | Abstract -> [%expr failwith "Abstract type cannot be constructed"]
                      | Open _ -> [%expr
                          let type_def_info =
                            Ocaml_in_python_api.IntHashtbl.find
                              Ocaml_in_python_api.OpenType.table
                              [%e Metapp.Exp.of_int type_info.index] in
                          let types =
                            List.map Ocaml_in_python_api.Type.of_index
                              [%e Metapp.Exp.list
                                  (List.map Metapp.Exp.of_int params_indexes)] in
                          type_def_info.make_api types;
                          let api_var =
                            Ocaml_in_python_api.TypeList.Hashtbl.find type_def_info.api_table
                              types in
                          Py.Callable.to_function_as_tuple
                            (Py.Object.find_attr_string v "_set_api")
                            (Py.Tuple.singleton
                               (api_var.api.(Py.Int.to_int (Py.Object.find_attr_string v "_constructor_index"))))]
                      | Record _ -> [%expr
                          let type_def_info =
                            Ocaml_in_python_api.IntHashtbl.find
                              Ocaml_in_python_api.type_def_table
                              [%e Metapp.Exp.of_int type_info.index] in
                          let types =
                            List.map Ocaml_in_python_api.Type.of_index
                              [%e Metapp.Exp.list
                                  (List.map Metapp.Exp.of_int params_indexes)] in
                          let api_var =
                            Ocaml_in_python_api.TypeList.Hashtbl.find type_def_info.api_table
                              types in
                          Py.Callable.to_function_as_tuple
                            (Py.Object.find_attr_string v "_set_api")
                            (Py.Tuple.singleton api_var.api)]
                      | Variant _ -> [%expr
                          let type_def_info =
                            Ocaml_in_python_api.IntHashtbl.find
                              Ocaml_in_python_api.variant_table
                              [%e Metapp.Exp.of_int type_info.index] in
                          let types =
                            List.map Ocaml_in_python_api.Type.of_index
                              [%e Metapp.Exp.list
                                  (List.map Metapp.Exp.of_int params_indexes)] in
                          type_def_info.make_api types;
                          let api_var =
                            Ocaml_in_python_api.TypeList.Hashtbl.find type_def_info.api_table
                              types in
                          Py.Callable.to_function_as_tuple
                            (Py.Object.find_attr_string v "_set_api")
                            (Py.Tuple.singleton
                               (api_var.api.(Py.Int.to_int (Py.Object.find_attr_string v "_constructor_index"))))]]
                else
                  capsule in
              snd [%e capsule_var] capsule
          else [%e
            let raise_error = [%expr
              raise (Py.Err (TypeError,
                Format.sprintf "%s expected but %s given"
                  [%e Metapp.Exp.of_string (Format.asprintf "%a" Pprintast.longident
                    type_info.longident)]
                  (Py.Type.name (Py.Type.get v))))] in
            match type_info.kind with
            | Record labels -> [%expr
                if Py.Type.get v = Dict then [%e
                  let vars = Type.Vars.create () in
                  List.iter2 (Type.Vars.bind vars)
                    type_info.type_declaration.type_params params;
                  Metapp.Exp.record (labels |> List.map (fun (label : _ LabelInfo.t) ->
                    let ty = Type.of_type_expr vars ocaml_env expansions label.ty in
                    let converter = Type.to_value_converter ocaml_env expansions ty in
                    Longident.Lident label.name,
                    Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                      [%expr Py.Dict.find_string v [%e Metapp.Exp.of_string label.name]]))]
                else
                  [%e raise_error]]
            | _ -> raise_error]];
      index in
  let python_of_ocaml = get_variable_ident python_of_ocaml converter_index in
  let ocaml_of_python = get_variable_ident ocaml_of_python converter_index in {
    python_of_ocaml = Ocaml_in_python_api.Function.Implicit (Metapp.Exp.ident python_of_ocaml);
    ocaml_of_python = Ocaml_in_python_api.Function.Implicit (Metapp.Exp.ident ocaml_of_python)}

let add_type_converter (type_info : type_info) =
  Types.Uid.Tbl.add type_constr_converter_tbl
    type_info.type_declaration.type_uid (make_type_converter type_info)

let add_abstract_type_info _ocaml_env _expansions _python_module (type_info : type_info) =
  let table = Ocaml_in_python_api.TypeList.Hashtbl.create 16 in
  let type_of_params params =
    Ppxlib.Ast_helper.Typ.constr
      (Metapp.mkloc type_info.longident)
      (List.map Ocaml_in_python_api.Type.to_core_type params) in
  let capsule_var params =
    try
      Ocaml_in_python_api.TypeList.Hashtbl.find table params
    with Not_found ->
      let class_index = count class_count in
      let capsule_class_var = Format.asprintf "class%d" class_index in
      let capsule_var = fresh_capsule_index () in
      let ty = type_of_params params in
      push_capsule_declaration (get_local_capsule_var capsule_var.local_index)
        (Metapp.Exp.of_string capsule_class_var) ty;
      Ocaml_in_python_api.TypeList.Hashtbl.add table params capsule_var;
      capsule_var in
  Ocaml_in_python_api.IntHashtbl.add Ocaml_in_python_api.type_def_table
    type_info.index {
      make_capsule = (fun params -> ignore (capsule_var params));
      make_api = (fun _params -> ());
      api_table = Ocaml_in_python_api.TypeList.Hashtbl.create 16; };
  type_info.capsule_var <- capsule_var

let check_public (type_info : type_info) f =
  match type_info.type_declaration.type_private with
  | Public -> f ()
  | Private ->
      [%expr raise (Py.Err (RuntimeError,
        (Printf.sprintf "Cannot create values of the private type %s"
          [%e Metapp.Exp.of_string type_info.name])))]

let add_record_type_info ocaml_env expansions _python_module (type_info : type_info)
      labels =
  let table = Ocaml_in_python_api.TypeList.Hashtbl.create 16 in
  let type_of_params params =
    Ppxlib.Ast_helper.Typ.constr
      (Metapp.mkloc type_info.longident)
      (List.map Ocaml_in_python_api.Type.to_core_type params) in
  let capsule_var params =
    try
      Ocaml_in_python_api.TypeList.Hashtbl.find table params
    with Not_found ->
      let class_index = count class_count in
      let capsule_class_var = Format.asprintf "class%d" class_index in
      let capsule_var = fresh_capsule_index () in
      Ocaml_in_python_api.TypeList.Hashtbl.add table params capsule_var;
      let ty = type_of_params params in
      push_capsule_declaration (get_local_capsule_var capsule_var.local_index)
        (Metapp.Exp.of_string capsule_class_var) ty;
      capsule_var in
  let api_table : Py.Object.t Ocaml_in_python_api.api Ocaml_in_python_api.TypeList.Hashtbl.t =
    Ocaml_in_python_api.TypeList.Hashtbl.create 16 in
  let make_api params =
    try
      ignore (Ocaml_in_python_api.TypeList.Hashtbl.find api_table params);
      ()
    with Not_found ->
      let _index = prepare_compilation_immediate () in
      let capsule_var =
        Ocaml_in_python_api.TypeList.Hashtbl.find table params in
      let class_index = count class_count in
      let vars = Type.Vars.create () in
      let add_var (var_name : Types.type_expr) arg =
        Type.Vars.bind vars var_name arg in
      List.iter2 add_var type_info.type_declaration.type_params params;
      let api_var = Format.asprintf "api%d" class_index in
      let labels = labels |> List.map
        (fun (info : _ LabelInfo.t) ->
          let ty = Type.of_type_expr vars ocaml_env expansions info.declaration.ld_type in
          info, ty, Type.to_value_converter ocaml_env expansions ty) in
      let access_field (info : _ LabelInfo.t) (converter : Ocaml_in_python_api.value_converter) =
        Ocaml_in_python_api.Function.apply converter.python_of_ocaml
          (Ppxlib.Ast_helper.Exp.field [%expr capsule]
            (Metapp.mklid info.name)) in
      let capsule_var' =
        Metapp.Exp.ident (get_variable_ident get_local_capsule_var capsule_var) in
      let make_record f =
        check_public type_info (fun () -> f (fun get_item -> [%expr
            fst [%e capsule_var']
                  [%e Metapp.Exp.record (labels |> List.mapi (fun i
                    ((info : _ LabelInfo.t), _ty, (converter : Ocaml_in_python_api.value_converter)) ->
                      Longident.Lident info.name,
                      Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                        (get_item i)))]])) in
      push_structure [%str
        let [%p Metapp.Pat.var api_var] =
          Py.Class.init [%e Metapp.Exp.of_string type_info.name]
            ~fields:[
              "make",
              Py.Callable.of_function_as_tuple (fun tuple ->
                [%e make_record (fun f -> [%expr
                  let template_item = Py.Tuple.get tuple 1 in
                  [%e f (fun _ -> [%expr template_item])]])]);
              "make_from_sequence",
              Py.Callable.of_function_as_tuple (fun tuple ->
                [%e make_record (fun f -> [%expr
                  let sequence = Py.Tuple.get tuple 0 in
                  [%e f (fun i -> [%expr
                  Py.Tuple.get sequence [%e Metapp.Exp.of_int i]])]])]);
              "length", Py.Callable.of_function_as_tuple (fun _tuple ->
                [%e make_python_int (List.length labels)]);
              "getitem", Py.Callable.of_function_as_tuple (fun tuple ->
                let capsule = snd [%e capsule_var']
                  (Py.Tuple.get tuple 0) in [%e
                Ppxlib.Ast_helper.Exp.match_
                  [%expr Py.Int.to_int (Py.Tuple.get tuple 1)]
                  ((labels |> List.mapi (fun i
                    ((info : _ LabelInfo.t), _ty, converter) ->
                      Ppxlib.Ast_helper.Exp.case (Metapp.Pat.of_int i)
                        (access_field info converter))) @
                     [Ppxlib.Ast_helper.Exp.case [%pat? index] [%expr
                       Ocaml_in_python_api.raise_index_out_of_bounds ~index
                         ~length:[%e Metapp.Exp.of_int
                           (List.length labels)]]])]);
              "setitem", Py.Callable.of_function_as_tuple (fun tuple -> [%e
                Ppxlib.Ast_helper.Exp.match_
                  [%expr Py.Int.to_int (Py.Tuple.get tuple 1)]
                  ((labels |> List.mapi (fun i
                    ((info : _ LabelInfo.t), _ty, (converter : Ocaml_in_python_api.value_converter)) ->
                      Ppxlib.Ast_helper.Exp.case (Metapp.Pat.of_int i)
                        (match info.declaration.ld_mutable with
                         | Immutable ->
                             [%expr raise (Py.Err (AttributeError, [%e
                               Metapp.Exp.of_string (Printf.sprintf
                                 "The record field %s is not mutable"
                                 info.name)]))]
                         | Mutable ->
                        [%expr
                let capsule = snd [%e capsule_var']
                  (Py.Tuple.get tuple 0) in [%e
                          Ppxlib.Ast_helper.Exp.setfield [%expr capsule]
                            (Metapp.mklid info.name)
                            (Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                              [%expr Py.Tuple.get tuple 2])];
                           Py.none]))) @
                     [Ppxlib.Ast_helper.Exp.case [%pat? index] [%expr
                       Ocaml_in_python_api.raise_index_out_of_bounds ~index
                         ~length:[%e Metapp.Exp.of_int
                           (List.length labels)]]])])]];
      let params_indexes =
        List.map Ocaml_in_python_api.Type.to_index params in
      let class_var = get_variable get_local_class_var type_info.class_var in
      push_structure [%str
        let type_def_info =
          Ocaml_in_python_api.IntHashtbl.find
            Ocaml_in_python_api.type_def_table
            [%e Metapp.Exp.of_int type_info.index] in
        let make capsule =
          Py.Callable.to_function_as_tuple_and_dict
            [%e class_var] Py.Tuple.empty
           (Py.Dict.of_bindings_string [
             ("__capsule", capsule);
             ("api", [%e Metapp.Exp.var api_var])]) in
        Ocaml_in_python_api.TypeList.Hashtbl.add type_def_info.api_table
          (List.map Ocaml_in_python_api.Type.of_index
            [%e Metapp.Exp.list
              (List.map Metapp.Exp.of_int params_indexes)])
          { api = [%e Metapp.Exp.var api_var]; make }];
      catch_compiler_errors (fun () -> perform_compilation ()) in
  Ocaml_in_python_api.IntHashtbl.add Ocaml_in_python_api.type_def_table
    type_info.index {
      make_capsule = (fun params -> ignore (capsule_var params));
      make_api;
      api_table; };
  type_info.capsule_var <- capsule_var;
  let class_var = get_variable get_local_class_var type_info.class_var in
  push_structure [%str
    let type_def_info =
      Ocaml_in_python_api.IntHashtbl.find Ocaml_in_python_api.type_def_table
        [%e Metapp.Exp.of_int type_info.index] in
    Py.Object.set_attr_string [%e class_var] "_api_for_type"
      (Py.Callable.of_function_as_tuple (fun tuple ->
        let type_list =
          Py.List.to_list_map Ocaml_in_python_api.Type.of_python
            (Py.Tuple.get tuple 0) in
        let api =
        try
          Ocaml_in_python_api.TypeList.Hashtbl.find
            type_def_info.api_table type_list
        with Not_found ->
          type_def_info.make_capsule type_list;
          Ocaml_in_python_api.TypeList.Hashtbl.find
            type_def_info.api_table type_list in api.api))]

let add_variant_type_info ocaml_env expansions python_module (type_info : type_info)
      (constructors : _ Constructor.t list) =
  constructors |> List.iter (fun (info : _ Constructor.t) ->
    push_structure
      [%str Py.Module.set [%e python_module]
        [%e Metapp.Exp.of_string info.name]
        [%e Metapp.Exp.ident (get_variable_ident get_local_class_var info.class_var)]
      ]);
  let table = Ocaml_in_python_api.TypeList.Hashtbl.create 16 in
  let type_of_params params =
    Ppxlib.Ast_helper.Typ.constr
      (Metapp.mkloc type_info.longident)
      (List.map Ocaml_in_python_api.Type.to_core_type params) in
  let capsule_var params =
    try
      Ocaml_in_python_api.TypeList.Hashtbl.find table params
    with Not_found ->
      let class_index = count class_count in
      let capsule_class_var = Format.asprintf "class%d" class_index in
      let capsule_var = fresh_capsule_index () in
      Ocaml_in_python_api.TypeList.Hashtbl.add table params capsule_var;
      let ty = type_of_params params in
      push_capsule_declaration (get_local_capsule_var capsule_var.local_index)
        (Metapp.Exp.of_string capsule_class_var) ty;
      capsule_var in
  let api_table : Py.Object.t array Ocaml_in_python_api.api Ocaml_in_python_api.TypeList.Hashtbl.t =
    Ocaml_in_python_api.TypeList.Hashtbl.create 16 in
  let make_api params =
    try
      ignore (Ocaml_in_python_api.TypeList.Hashtbl.find api_table params);
      ()
    with Not_found ->
      let _index = prepare_compilation_immediate () in
      let capsule_var =
        Ocaml_in_python_api.TypeList.Hashtbl.find table params in
      let ty = type_of_params params in
      let class_index = count class_count in
      let vars = Type.Vars.create () in
      let add_var (var_name : Types.type_expr) arg =
        Type.Vars.bind vars var_name arg in
      List.iter2 add_var type_info.type_declaration.type_params params;
      let api_var = Format.asprintf "api%d" class_index in
      let constructors = constructors |>
        List.map (Constructor.map (fun ty ->
          let ty = Type.of_type_expr vars ocaml_env expansions ty in
          ty, Type.to_value_converter ocaml_env expansions ty)) in
      let capsule_var =
        Metapp.Exp.ident (get_variable_ident get_local_capsule_var capsule_var) in
      push_structure [%str
        let [%p Metapp.Pat.var api_var] = [%e
          Ppxlib.Ast_helper.Exp.array (constructors |> List.map
            (fun (cstr : _ Constructor.t) ->
              match cstr.result with
              | Some (Ocaml_in_python_api.Type.Constr (_, result_params), _) when not (List.equal Ocaml_in_python_api.Type.equal result_params params) -> [%expr Py.none]
              | _ ->
              let make f =
                check_public type_info (fun () ->
                  f (fun f ->
                let args =
                  match cstr.args with
                  | Tuple args ->
                      let make_arg i (ty, converter) =
                        f i None ty converter in
                      List.mapi make_arg args
                  | Record labels ->
                      let labels =
                        labels |> List.mapi (fun i (info : _ LabelInfo.t) :
                          (Longident.t * Ppxlib.expression) ->
                          let ty, converter = info.ty in
                          Lident info.name,
                          f i (Some info.name) ty converter) in
                      [Metapp.Exp.record labels] in
                [%expr
                fst [%e capsule_var] ([%e Metapp.Exp.construct
                  (Lident cstr.name) args] : [%t ty])])) in
              let destruct_args =
                match cstr.args with
                | Tuple args ->
                    args |> List.mapi (fun i _ ->
                      Metapp.Pat.var (Printf.sprintf "f%d" i))
                | Record labels ->
                    [Metapp.Pat.record (
                      labels |> List.mapi (fun i (info : _ LabelInfo.t) :
                        (Longident.t  * Ppxlib.pattern) ->
                        Lident info.name,
                        Metapp.Pat.var (Printf.sprintf "f%d" i)))] in
              let destruct_pat =
                Metapp.Pat.construct (Lident cstr.name) destruct_args in
              let destruct destruct_pat body =
                match constructors with
                | [_] -> [%expr
                    match capsule with [%p destruct_pat] -> [%e body]]
                | _ -> [%expr
                    match capsule with
                    | [%p destruct_pat] -> [%e body]
                    | _ -> failwith "destruct"] in [%expr
              Py.Class.init [%e Metapp.Exp.of_string
                  (Printf.sprintf "%s.%s" type_info.name cstr.name)]
                ~fields:[
                  "make",
                  Py.Callable.of_function_as_tuple (fun tuple ->
                    [%e
                    (make (fun f -> (fun e ->
                          if cstr.args = Tuple [] then
                            e
                          else
                            [%expr let template_item = Py.Tuple.get tuple 1 in [%e e]])
(f (fun _i _label _ty (converter : Ocaml_in_python_api.value_converter) ->
                      Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                            [%expr template_item]))))]);
                  "make_from_sequence",
                  Py.Callable.of_function_as_tuple (fun tuple ->
                    [%e (make (fun f -> (fun e ->
                      if cstr.args = Tuple [] then
                        e
                      else
                        [%expr let sequence = Py.Tuple.get tuple 0 in [%e e]])
                    (f (fun i _label _ty (converter : Ocaml_in_python_api.value_converter) ->
                      Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                        [%expr Py.Tuple.get sequence [%e Metapp.Exp.of_int i]]))))]);
                  "length", Py.Callable.of_function_as_tuple (fun _tuple ->
                    [%e make_python_int (ConstructorArgs.length cstr.args)]);
                  "getitem", Py.Callable.of_function_as_tuple (fun tuple ->
                    let capsule = snd [%e capsule_var]
                      (Py.Tuple.get tuple 0) in
                    [%e destruct destruct_pat (
                        Ppxlib.Ast_helper.Exp.match_
                          [%expr Py.Int.to_int (Py.Tuple.get tuple 1)]
                          ((cstr.args |> ConstructorArgs.to_list_mapi (fun i (_ty, (converter : Ocaml_in_python_api.value_converter)) ->
                            Ppxlib.Ast_helper.Exp.case (Metapp.Pat.of_int i) [%expr
                              [%e (Ocaml_in_python_api.Function.apply
                                converter.python_of_ocaml
                                (Metapp.Exp.var (Printf.sprintf "f%d" i)))]])) @
                          [Ppxlib.Ast_helper.Exp.case [%pat? index] [%expr
                            Ocaml_in_python_api.raise_index_out_of_bounds ~index
                              ~length:[%e Metapp.Exp.of_int
                                (ConstructorArgs.length cstr.args)]]]))]);
                  "setitem", Py.Callable.of_function_as_tuple (fun tuple -> [%e
                    match cstr.args with
                    | Tuple _ -> [%expr
                        raise (Py.Err (AttributeError, [%e
                          Metapp.Exp.of_string (Printf.sprintf
                            "The arguments of constructor %s are not mutable"
                            cstr.name)]))]
                    | Record labels -> [%expr
                        let capsule = snd [%e capsule_var]
                          (Py.Tuple.get tuple 0) in
                        [%e destruct (Metapp.Pat.construct (Lident cstr.name)
                              [[%pat? r]]) (
                            Ppxlib.Ast_helper.Exp.match_
                              [%expr Py.Int.to_int (Py.Tuple.get tuple 1)]
                                ((labels |> List.mapi (fun i (info : _ LabelInfo.t) ->
                                  let _, (converter : Ocaml_in_python_api.value_converter) = info.ty in
                                  Ppxlib.Ast_helper.Exp.case (Metapp.Pat.of_int i)
                                  (match info.declaration.ld_mutable with
                                  | Immutable ->
                                      [%expr raise (Py.Err (AttributeError, [%e
                                        Metapp.Exp.of_string (Printf.sprintf
                                          "The record field %s is not mutable"
                                          info.name)]))]
                                  | Mutable ->
                                    [%expr [%e
                                      Ppxlib.Ast_helper.Exp.setfield [%expr r]
                                        (Metapp.mklid info.name)
                                        (Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                                          [%expr Py.Tuple.get tuple 2])];
                                       Py.none]))) @
                               [Ppxlib.Ast_helper.Exp.case [%pat? index] [%expr
                                 Ocaml_in_python_api.raise_index_out_of_bounds ~index
                                   ~length:[%e Metapp.Exp.of_int
                                     (List.length labels)]]]))]]])]]))]];
      let params_indexes =
        List.map Ocaml_in_python_api.Type.to_index params in
      push_structure [%str
        let type_def_info =
          try
          Ocaml_in_python_api.IntHashtbl.find
            Ocaml_in_python_api.variant_table
            [%e Metapp.Exp.of_int type_info.index]
          with Not_found -> failwith "type_def_info" in
        let make capsule = [%e
          Ppxlib.Ast_helper.Exp.match_
            [%expr snd [%e capsule_var] capsule]
            (constructors |> List.mapi
              (fun i (cstr : _ Constructor.t) ->
              match cstr.result with
              | Some (Ocaml_in_python_api.Type.Constr (_, result_params), _) when not (List.equal Ocaml_in_python_api.Type.equal result_params params) -> None
              | _ -> Some (
                Ppxlib.Ast_helper.Exp.case
                  (Metapp.Pat.construct (Lident cstr.name)
                    (if cstr.args = Tuple [] then [] else [[%pat? _]]))
                [%expr Py.Callable.to_function_as_tuple_and_dict
                  [%e Metapp.Exp.ident (get_variable_ident get_local_class_var cstr.class_var)] Py.Tuple.empty
                  (Py.Dict.of_bindings_string [
                    ("__capsule", capsule);
                    ("api", [%e Metapp.Exp.var api_var].
                      ([%e Metapp.Exp.of_int i]))])])) |> List.filter_map Fun.id)] in
        let params =
          List.map Ocaml_in_python_api.Type.of_index
            [%e Metapp.Exp.list
              (List.map Metapp.Exp.of_int params_indexes)] in
        Ocaml_in_python_api.TypeList.Hashtbl.add type_def_info.api_table
          params
          { api = [%e Metapp.Exp.var api_var]; make }];
      catch_compiler_errors (fun () -> perform_compilation ()) in
  Ocaml_in_python_api.IntHashtbl.add Ocaml_in_python_api.variant_table
    type_info.index {
      make_capsule = (fun params -> ignore (capsule_var params));
      make_api;
      api_table };
  type_info.capsule_var <- capsule_var;
  push_structure [%str
    let type_def_info =
      try
        Ocaml_in_python_api.IntHashtbl.find Ocaml_in_python_api.variant_table
          [%e Metapp.Exp.of_int type_info.index]
      with Not_found -> failwith "type_def_info" in
    [%e Metapp.sequence (
      constructors |> List.mapi (fun i (cstr : _ Constructor.t) -> [%expr
        Py.Object.set_attr_string [%e Metapp.Exp.ident (get_variable_ident get_local_class_var cstr.class_var)]
          "_api_for_type"
          (Py.Callable.of_function_as_tuple (fun tuple ->
            let type_list =
              Py.List.to_list_map Ocaml_in_python_api.Type.of_python
                (Py.Tuple.get tuple 0) in
            let api =
              try
                Ocaml_in_python_api.TypeList.Hashtbl.find
                  type_def_info.api_table type_list
              with Not_found ->
                type_def_info.make_capsule type_list;
                Ocaml_in_python_api.TypeList.Hashtbl.find
                  type_def_info.api_table type_list in
            api.api.([%e Metapp.Exp.of_int i])))]))]]

let add_open_type_info ocaml_env expansions _python_module (type_info : type_info) constructors =
  let table = Ocaml_in_python_api.TypeList.Hashtbl.create 16 in
  let type_of_params params =
    Ppxlib.Ast_helper.Typ.constr
      (Metapp.mkloc type_info.longident)
      (List.map Ocaml_in_python_api.Type.to_core_type params) in
  let capsule_var params =
    try
      Ocaml_in_python_api.TypeList.Hashtbl.find table params
    with Not_found ->
      let class_index = count class_count in
      let capsule_class_var = Format.asprintf "class%d" class_index in
      let capsule_var = fresh_capsule_index () in
      let ty = type_of_params params in
      push_capsule_declaration (get_local_capsule_var capsule_var.local_index)
        (Metapp.Exp.of_string capsule_class_var) ty;
      Ocaml_in_python_api.TypeList.Hashtbl.add table params capsule_var;
      capsule_var in
  let api_table : Py.Object.t array Ocaml_in_python_api.api Ocaml_in_python_api.TypeList.Hashtbl.t =
    Ocaml_in_python_api.TypeList.Hashtbl.create 16 in
  let make_api params =
    try
      ignore (Ocaml_in_python_api.TypeList.Hashtbl.find api_table params);
      ()
    with Not_found ->
      let _index = prepare_compilation_immediate () in
      let capsule_var =
        Ocaml_in_python_api.TypeList.Hashtbl.find table params in
      let ty = type_of_params params in
      let class_index = count class_count in
      let vars = Type.Vars.create () in
      let add_var (var_name : Types.type_expr) arg =
        Type.Vars.bind vars var_name arg in
      List.iter2 add_var type_info.type_declaration.type_params params;
      let api_var = Format.asprintf "api%d" class_index in
      let constructors =
        constructors |> Ocaml_in_python_api.ExtensibleArray.to_list_map (fun cstr ->
        cstr |> Option.get |>
        Constructor.map (fun ty ->
          ty, Type.to_value_converter ocaml_env expansions ty)) in
      let capsule_var =
        Metapp.Exp.ident (get_variable_ident get_local_capsule_var capsule_var) in
      push_structure [%str
        let [%p Metapp.Pat.var api_var] = [%e
          Ppxlib.Ast_helper.Exp.array (constructors |> List.map (fun (cstr : _ Constructor.t) ->
              match cstr.result with
              | Some (Ocaml_in_python_api.Type.Constr (_, result_params), _) when not (List.equal Ocaml_in_python_api.Type.equal result_params params) -> [%expr Py.none]
              | _ ->
              let make f =
                let args =
                  match cstr.args with
                  | Tuple args ->
                      let make_arg i (ty, converter) =
                        f i None ty converter in
                      List.mapi make_arg args
                  | Record labels ->
                      let labels =
                        labels |> List.mapi (fun i (info : _ LabelInfo.t) :
                          (Longident.t * Ppxlib.expression) ->
                          let ty, converter = info.ty in
                          Lident info.name,
                          f i (Some info.name) ty converter) in
                      [Metapp.Exp.record labels] in
                [%expr
                fst [%e capsule_var] ([%e Metapp.Exp.construct
                  cstr.name args] : [%t ty])] in
              let destruct_args =
                match cstr.args with
                | Tuple args ->
                    args |> List.mapi (fun i _ ->
                      Metapp.Pat.var (Printf.sprintf "f%d" i))
                | Record labels ->
                    [Metapp.Pat.record (
                      labels |> List.mapi (fun i (info : _ LabelInfo.t) :
                        (Longident.t  * Ppxlib.pattern) ->
                        Lident info.name,
                        Metapp.Pat.var (Printf.sprintf "f%d" i)))] in
              let destruct_pat =
                Metapp.Pat.construct cstr.name destruct_args in [%expr
              Py.Class.init [%e Metapp.Exp.of_string
                  (Format.asprintf "%s.%a" type_info.name Pprintast.longident cstr.name)]
                ~fields:[
                  "make",
                  Py.Callable.of_function_as_tuple (fun tuple ->
                    [%e (fun e ->
                          if cstr.args = Tuple [] then
                            e
                          else
                            [%expr let template_item = Py.Tuple.get tuple 1 in [%e e]])
                    (make (fun _i _label _ty (converter : Ocaml_in_python_api.value_converter) ->
                      Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                            [%expr template_item]))]);
                  "make_from_sequence",
                  Py.Callable.of_function_as_tuple (fun tuple ->
                    [%e (fun e ->
                      if cstr.args = Tuple [] then
                        e
                      else
                        [%expr let sequence = Py.Tuple.get tuple 0 in [%e e]])
                    (make (fun i _label _ty (converter : Ocaml_in_python_api.value_converter) ->
                      Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                        [%expr Py.Tuple.get sequence [%e Metapp.Exp.of_int i]]))]);
                  "length", Py.Callable.of_function_as_tuple (fun _tuple ->
                     [%e make_python_int (ConstructorArgs.length cstr.args)]);
                  "getitem", Py.Callable.of_function_as_tuple (fun tuple ->
                    let capsule = snd [%e capsule_var]
                      (Py.Tuple.get tuple 0) in
                    match capsule with
                    | [%p destruct_pat ] -> [%e
                        Ppxlib.Ast_helper.Exp.match_
                          [%expr Py.Int.to_int (Py.Tuple.get tuple 1)]
                          ((cstr.args |> ConstructorArgs.to_list_mapi (fun i (_ty, (converter : Ocaml_in_python_api.value_converter)) ->
                            Ppxlib.Ast_helper.Exp.case (Metapp.Pat.of_int i) [%expr
                              [%e (Ocaml_in_python_api.Function.apply
                                converter.python_of_ocaml
                                (Metapp.Exp.var (Printf.sprintf "f%d" i)))]])) @
                          [Ppxlib.Ast_helper.Exp.case [%pat? index] [%expr
                            Ocaml_in_python_api.raise_index_out_of_bounds ~index
                              ~length:[%e Metapp.Exp.of_int
                                (ConstructorArgs.length cstr.args)]]])]
                    | _ -> failwith "getitem");
                  "setitem", Py.Callable.of_function_as_tuple (fun tuple -> [%e
                    match cstr.args with
                    | Tuple _ -> [%expr
                        raise (Py.Err (AttributeError, [%e
                          Metapp.Exp.of_string (Format.asprintf
                            "The arguments of constructor %a are not mutable"
                            Pprintast.longident cstr.name)]))]
                    | Record labels -> [%expr
                        let capsule = snd [%e capsule_var]
                          (Py.Tuple.get tuple 0) in
                        match capsule with
                        | [%p Metapp.Pat.construct cstr.name
                            [[%pat? r]]] -> [%e
                            Ppxlib.Ast_helper.Exp.match_
                              [%expr Py.Int.to_int (Py.Tuple.get tuple 1)]
                                ((labels |> List.mapi (fun i (info : _ LabelInfo.t) ->
                                  let _, (converter : Ocaml_in_python_api.value_converter) = info.ty in
                                  Ppxlib.Ast_helper.Exp.case (Metapp.Pat.of_int i)
                                  (match info.declaration.ld_mutable with
                                  | Immutable ->
                                      [%expr raise (Py.Err (AttributeError, [%e
                                        Metapp.Exp.of_string (Printf.sprintf
                                          "The record field %s is not mutable"
                                          info.name)]))]
                                  | Mutable ->
                                    [%expr [%e
                                      Ppxlib.Ast_helper.Exp.setfield [%expr r]
                                        (Metapp.mklid info.name)
                                        (Ocaml_in_python_api.Function.apply converter.ocaml_of_python
                                          [%expr Py.Tuple.get tuple 2])];
                                       Py.none]))) @
                               [Ppxlib.Ast_helper.Exp.case [%pat? index] [%expr
                                 Ocaml_in_python_api.raise_index_out_of_bounds ~index
                                   ~length:[%e Metapp.Exp.of_int
                                     (List.length labels)]]])]
                        | _ -> failwith "setitem"]])]]))]];
      let params_indexes =
        List.map Ocaml_in_python_api.Type.to_index params in
      push_structure [%str
        let type_def_info =
          try
            Ocaml_in_python_api.IntHashtbl.find
              Ocaml_in_python_api.OpenType.table
              [%e Metapp.Exp.of_int type_info.index]
          with Not_found -> failwith "type_def_info" in
        let make capsule = [%e
          Ppxlib.Ast_helper.Exp.match_
            [%expr snd [%e capsule_var] capsule]
            ((constructors |> List.mapi
              (fun i (cstr : _ Constructor.t) ->
              match cstr.result with
              | Some (Ocaml_in_python_api.Type.Constr (_, result_params), _) when not (List.equal Ocaml_in_python_api.Type.equal result_params params) -> None
              | _ -> Some (
                Ppxlib.Ast_helper.Exp.case
                  (Metapp.Pat.construct cstr.name
                    (if cstr.args = Tuple [] then [] else [[%pat? _]]))
                [%expr Py.Callable.to_function_as_tuple_and_dict
                  [%e Metapp.Exp.ident (get_variable_ident get_local_class_var cstr.class_var)] Py.Tuple.empty
                  (Py.Dict.of_bindings_string [
                    ("__capsule", capsule);
                    ("api", [%e Metapp.Exp.var api_var].
                      ([%e Metapp.Exp.of_int i]))])])) |> List.filter_map Fun.id) @
               [Ppxlib.Ast_helper.Exp.case [%pat? obj]
                  [%expr failwith ("make " ^
                    Obj.Extension_constructor.(name (of_val obj)))]])] in
        let params =
          List.map Ocaml_in_python_api.Type.of_index
            [%e Metapp.Exp.list
              (List.map Metapp.Exp.of_int params_indexes)] in
        Ocaml_in_python_api.TypeList.Hashtbl.add type_def_info.api_table
          params
          { api = [%e Metapp.Exp.var api_var]; make }];
      catch_compiler_errors (fun () -> perform_compilation ()) in
  Ocaml_in_python_api.IntHashtbl.add Ocaml_in_python_api.OpenType.table
    type_info.index {
      make_capsule = (fun params -> ignore (capsule_var params));
      make_api;
      api_table };
  type_info.capsule_var <- capsule_var

let add_type_info ocaml_env expansions python_module (type_info : type_info) =
  let class_var = get_variable get_local_class_var type_info.class_var in
  push_structure
    [%str Py.Module.set [%e python_module]
      [%e Metapp.Exp.of_string type_info.name]
      [%e class_var]
    ];
  match type_info.kind with
  | Abstract ->
      add_abstract_type_info ocaml_env expansions python_module type_info
  | Record labels ->
      add_record_type_info ocaml_env expansions python_module type_info labels
  | Variant cstrs ->
      add_variant_type_info ocaml_env expansions python_module type_info cstrs
  | Open cstrs ->
      add_open_type_info ocaml_env expansions python_module type_info cstrs

let table_count = ref 0

let polymorphic_function_converter ~name ocaml_env expansions (vars : Type.Vars.t) ty ident : Ppxlib.expression =
  let arity = Type.arity_of_type ty in
  let concrete_type_names =
    String.concat ", " (vars.names |>
      Ocaml_in_python_api.ExtensibleArray.to_list_map (fun name ->
        Option.value ~default:"_" name)) in
  let type_names =
    List.init vars.names.length (fun i -> Printf.sprintf "type%d" i) in
  let pat_type_names =
    type_names |> List.map (fun s -> Metapp.Pat.var s) in
  let exp_type_names =
    type_names |> List.map (fun s -> Metapp.Exp.var s) in
  let type_none_tuple =
    Metapp.Exp.tuple (type_names |> List.map (fun _ -> [%expr None])) in
  let table = Ocaml_in_python_api.TypeList.Hashtbl.create 16 in
  let index = Ocaml_in_python_api.PolymorphicFunction.push (fun index ->
    let make types =
      try
        Ocaml_in_python_api.TypeList.Hashtbl.find table types
      with Not_found ->
        let types_array = Array.of_list types in
        let subst_vars index =
          types_array.(index) in
        let arity = Type.map_arity (Type.subst subst_vars) arity in
        let _compile_index = prepare_compilation_immediate () in
        let ({ ocaml_exps; _ } : Ocaml_in_python_api.converters_of_arity) =
          Type.converters_of_arity ocaml_env expansions arity in
        let result_converter = Type.to_value_converter ocaml_env expansions arity.result in
        push_structure [%str
          let table =
            (Ocaml_in_python_api.PolymorphicFunction.get
              [%e Metapp.Exp.of_int index]).table in
          let types = List.map Ocaml_in_python_api.Type.of_index
            [%e Metapp.Exp.list (List.map (fun ty ->
               Metapp.Exp.of_int (Ocaml_in_python_api.Type.to_index ty))
               types)] in
          Ocaml_in_python_api.TypeList.Hashtbl.add table types
            (fun ~args_tuple ~keywords_dict -> [%e
              make_ocaml_function_call ocaml_env expansions arity result_converter
                ident ocaml_exps])];
        catch_compiler_errors (fun () ->
          perform_compilation ());
        Ocaml_in_python_api.TypeList.Hashtbl.find table types in
    { make; table }) in
  let vars_count = Type.Vars.count vars in
  let vars_count_exp = Metapp.Exp.of_int vars_count in
  [%expr
    let make =
      (Ocaml_in_python_api.PolymorphicFunction.get
        [%e Metapp.Exp.of_int index]).make in
    Py.Callable.of_function_as_tuple_and_dict
      ~name:[%e Metapp.Exp.of_string name] (fun args_tuple keywords_dict ->
        let [%p Metapp.Pat.tuple pat_type_names] =
          match
            if keywords_dict = Py.null then
              None
            else
              Py.Dict.get_item_string keywords_dict "type" with
          | None -> [%e type_none_tuple]
          | Some type_ ->
              begin match Py.Type.get type_ with
              | Tuple ->
                  if Py.Tuple.size type_ <> [%e vars_count_exp] then
                    raise (Py.Err (RuntimeError, Printf.sprintf
                      "%d types given but %d expected: %s"
                      (Py.Tuple.size type_) [%e vars_count_exp]
                      [%e Metapp.Exp.of_string concrete_type_names]));
                  [%e Metapp.Exp.tuple (type_names |> List.mapi (fun i _ ->
                    [%expr Some (Py.Tuple.get type_ [%e Metapp.Exp.of_int i])]))]
              | Dict ->
                  let add_key key value
                        [%p Metapp.Pat.tuple pat_type_names] =
                    [%e Ppxlib.Ast_helper.Exp.match_
                        [%expr Py.String.to_string key]
                        ((List.filter_map Fun.id (List.init vars_count (fun i ->
                          match Type.Vars.get_name vars i with
                          | None -> None
                          | Some var -> Some (
                          Ppxlib.Ast_helper.Exp.case
                            (Metapp.Pat.of_string var)
                            (Metapp.Exp.tuple (exp_type_names |> List.mapi
                              (fun j var ->
                                if i = j then
                                  [%expr Some value]
                                else
                                  var))))))) @
                          [Ppxlib.Ast_helper.Exp.case [%pat? s] [%expr
                            raise (Py.Err (RuntimeError, Printf.sprintf
                              "Unknown type variable '%s' (variables are: %s)" s
                                [%e Metapp.Exp.of_string concrete_type_names]))]])] in
                  Py.Dict.fold add_key type_ [%e type_none_tuple]
              | _ -> [%e
                  if vars_count = 1 then
                    [%expr Some type_]
                  else
                    [%expr
                    raise (Py.Err (RuntimeError, Printf.sprintf
                      "One type given but %d expected: %s"
                      [%e vars_count_exp]
                      [%e Metapp.Exp.of_string concrete_type_names]))]]
              end in
        let [%p Metapp.Pat.tuple pat_type_names] =
          [%e Metapp.Exp.tuple (exp_type_names |> List.map (fun var -> [%expr
            match [%e var] with
            | None -> Ocaml_in_python_api.Type.Any
            | Some ty -> Ocaml_in_python_api.Type.of_python ty]))] in
        let f = make [%e Metapp.Exp.list exp_type_names] in
        f ~args_tuple ~keywords_dict)]

let python_module_count = ref 0

let rec convert_signature_items ocaml_env expansions longident path python_module
      (list : Types.signature_item list) =
  match list with
  | [] -> ()
  | Sig_value (ident, value_description, _visibility) :: tail ->
      let name = Ident.name ident in
      let longident' = Longident.Ldot (longident, name) in
      begin try
        let vars = Type.Vars.create () in
        let ty = Type.of_type_expr vars ocaml_env expansions value_description.val_type in
        let ident =
          Ppxlib.Ast_helper.Exp.ident (Metapp.mkloc longident') in
        let expr =
          if Type.Vars.count vars = 0 then
            begin
            let converter = Type.to_value_converter ocaml_env expansions ~name ty in
            match ty with
            | Arrow _ ->
                Ocaml_in_python_api.Function.apply converter.python_of_ocaml
                  ident
            | _ ->
                [%expr Py.Callable.to_function_as_tuple (Py.Class.init "postponed"
                ~fields:["computed", Py.Bool.f; "value", Py.none]
                ~methods:["__get__", Py.Callable.of_function_as_tuple
                    (fun tuple ->
                      let self = Py.Tuple.get tuple 0 in
                      if Py.Bool.to_bool (Py.Object.find_attr_string self "computed")
                      then
                        Py.Object.find_attr_string self "value"
                      else
                        let value =
                          [%e Ocaml_in_python_api.Function.apply converter.python_of_ocaml
                                       ident] in
                        Py.Object.set_attr_string self "value" value;
                        value)]) Py.Tuple.empty]
            end
          else
            match ty with
            | Arrow _ ->
                polymorphic_function_converter ~name ocaml_env expansions vars ty ident
            | _ ->
                failwith "Polymorphic values are not supported" in
        push_structure [%str
          Py.Module.set [%e python_module] [%e Metapp.Exp.of_string name]
            [%e expr]]
      with exc ->
        if !debug then
          let format_exc fmt exc =
            match exc with
            | Env.Error error -> Env.report_error fmt error
            | _ -> Format.pp_print_string fmt (Printexc.to_string exc) in
          Format.eprintf "Warning: %a not available: %a@."
            Pprintast.longident longident' format_exc exc
      end;
      convert_signature_items ocaml_env expansions longident path python_module tail
  | Sig_type (ident, type_declaration, rec_status, _visibility) :: tail ->
      let type_declarations = [ident, type_declaration] in
      let type_declarations, tail =
        match rec_status with
        | Trec_not -> type_declarations, tail
        | Trec_first ->
            chop_other_type_declarations type_declarations tail
        | Trec_next -> assert false in
      let add_type_declaration_expansion expansions (ident, _) =
        Path.Map.add (Path.Pident ident)
          (Path.Pdot (path, Ident.name ident)) expansions in
      let expansions' =
        List.fold_left add_type_declaration_expansion expansions
          type_declarations in
      let expansions =
        match rec_status with
        | Trec_first -> expansions'
        | _ -> expansions in
      let type_declarations =
        List.filter (fun (_ident, (declaration : Types.type_declaration)) ->
          let () =
            try
              ignore (Ocaml_common.Env.find_type (Path.Pident ident) ocaml_env)
            with Not_found -> prerr_endline "Not found!" in
          not (Types.Uid.Tbl.mem type_constr_converter_tbl
            declaration.type_uid))
          type_declarations in
      let type_infos =
        List.map (add_class_prototype longident) type_declarations in
      List.iter add_type_converter type_infos;
      List.iter (add_type_info ocaml_env expansions python_module) type_infos;
      convert_signature_items ocaml_env expansions' longident path python_module tail
  | Sig_typext (ident, ext, _status, _visibility) :: tail ->
      let name = Ident.name ident in
      let longident' : Longident.t = Ldot (longident, name) in
      let cstr = Constructor.of_extension_constructor longident' ext in
      let type_path = Type.expand_path expansions ext.ext_type_path in
      let uid = uid_of_type_path ocaml_env type_path in
      let open_type = Types.Uid.Tbl.find open_types_tbl uid in
      let class_var = get_variable get_local_class_var open_type.class_var in
      let vars = Type.Vars.create () in
      let cstr = cstr |> Constructor.map (fun ty ->
        Type.of_type_expr vars ocaml_env expansions ty) in
      let index = Ocaml_in_python_api.ExtensibleArray.push open_type.constructors (Some cstr) in
      push_constructor_class longident' open_type.name class_var (fun (longident : Longident.t) -> Format.asprintf "%a" Pprintast.longident longident) index cstr;
      push_structure [%str
        Py.Module.set [%e python_module] [%e Metapp.Exp.of_string name]
          [%e Metapp.Exp.var (get_local_class_var cstr.class_var.local_index)]];
      push_structure [%str
        let type_def_info =
          Ocaml_in_python_api.IntHashtbl.find Ocaml_in_python_api.OpenType.table
            [%e Metapp.Exp.of_int open_type.index] in
        Ocaml_in_python_api.TypeList.Hashtbl.clear type_def_info.api_table;
        Py.Object.set_attr_string [%e Metapp.Exp.ident (get_variable_ident get_local_class_var cstr.class_var)]
              "_api_for_type"
              (Py.Callable.of_function_as_tuple (fun tuple ->
                let type_list =
                  Py.List.to_list_map Ocaml_in_python_api.Type.of_python
                    (Py.Tuple.get tuple 0) in
                let api =
                  try
                    Ocaml_in_python_api.TypeList.Hashtbl.find
                      type_def_info.api_table type_list
                  with Not_found ->
                    type_def_info.make_capsule type_list;
                    Ocaml_in_python_api.TypeList.Hashtbl.find
                      type_def_info.api_table type_list in
               api.api.([%e Metapp.Exp.of_int index])))];
      convert_signature_items ocaml_env expansions longident path python_module tail
  | Sig_module (ident, _presence, decl, _rec, _visibility) :: tail ->
      let name = Ident.name ident in
      let longident' = Longident.Ldot (longident, name) in
      if longident' <> Ldot (Lident "Stdlib", "Oo") then
        begin
          let path' = Path.Pdot (path, name) in
          match
            (python_of_module_declaration ocaml_env expansions
               longident' path' decl : Ocaml_in_python_api.Paths.index_cell option)
          with
          | Some { index; class_ = _ } ->
              push_structure [%str
                Py.Module.set [%e python_module] [%e Metapp.Exp.of_string name]
                 (Ocaml_in_python_api.Paths.get [%e Metapp.Exp.of_int index]).class_]
          | None -> ()
        end;
      convert_signature_items ocaml_env expansions longident path python_module tail
  | _ :: tail ->
      convert_signature_items ocaml_env expansions longident path python_module tail

and convert_signature ocaml_env expansions longident path path_index signature =
  let python_module_index = count python_module_count in
  let python_module_var =
    Printf.sprintf "python_module%d" python_module_index in
  push_preamble [%str
    let { class_ = [%p Metapp.Pat.var python_module_var]; path = _ } :
      Ocaml_in_python_api.Paths.path_cell =
      Ocaml_in_python_api.Paths.get [%e Metapp.Exp.of_int path_index]];
  let python_module = Metapp.Exp.var python_module_var in
  convert_signature_items ocaml_env expansions longident path
    python_module signature;
  pop_preample ();
  cut_compilation ()

and python_of_module_declaration ocaml_env expansions longident path
    (moddecl : Types.module_declaration) =
  match Ocaml_in_python_api.Paths.find_opt path with
  | Some _ as result -> result
  | None ->
      match moddecl.md_type with
      | Mty_ident _ ->
          None
      | Mty_signature signature ->
          let root = prepare_compilation_opt () in
          let ocaml_env' = Ocaml_common.Env.add_signature signature ocaml_env in
          let class_ = Py.Class.init (Path.name path) in
          let index = Ocaml_in_python_api.Paths.register path class_ in
          convert_signature ocaml_env' expansions longident path index
            signature;
          catch_compiler_errors (fun () ->
            Option.iter (fun _ -> perform_compilation ()) root);
          Some { index; class_ }
      | Mty_functor _ ->
          None
      | Mty_alias path ->
          python_of_module_path ocaml_env expansions longident path

and python_of_module_path ocaml_env expansions longident path =
  let moddecl = Ocaml_common.Env.find_module path ocaml_env in
  python_of_module_declaration ocaml_env expansions longident path moddecl

let python_of_module_name ocaml_env expansions name =
  if name = "Stdlib__Lexing" || name = "CamlinternalOO" then
    None
  else
  let longident : Longident.t = Lident name in
  let path, moddecl =
    Ocaml_common.Env.lookup_module ~loc:!Ppxlib.Ast_helper.default_loc
      longident ocaml_env in
  python_of_module_declaration ocaml_env expansions longident path moddecl

let value_converter_of_bytes : Ocaml_in_python_api.value_converter = {
  ocaml_of_python = Explicit (fun v -> [%expr
    if Py.Object.is_instance [%e v] (Py.Object.find_attr_string
      (Ocaml_in_python_api.get_root_python_module ()) "bytes") then
      begin
        let capsule = Py.Object.find_attr_string [%e v] "_capsule" in
        snd Ocaml_in_python_api.bytes_capsule capsule
      end
    else
      Bytes.of_string (Py.String.to_string [%e v])]);
  python_of_ocaml = Explicit (fun v -> [%expr
    let ocaml = Ocaml_in_python_api.get_root_python_module () in
    let bytes_class = Py.Module.get ocaml "bytes" in
    Py.Callable.to_function_as_tuple_and_dict bytes_class
      Py.Tuple.empty (Py.Dict.of_bindings_string [
        "__capsule", fst Ocaml_in_python_api.bytes_capsule [%e v];])]); }

let () =
  import_ocaml_module_in_python_ref := (fun ocaml_env (expansions : Path.t Path.Map.t) name ->
    let ({ class_; index = _ } : Ocaml_in_python_api.Paths.index_cell) =
      Option.get (python_of_module_name ocaml_env expansions name) in
    let ocaml = Ocaml_in_python_api.get_root_python_module () in
    Py.Module.set ocaml name class_;
    class_)

let initialize_python ocaml_env =
  Py.initialize ();
  let ocaml = Ocaml_in_python_api.get_root_python_module () in
  let register_primitive name f =
    Py.Module.set ocaml name (Py.Callable.of_function_as_tuple (fun tuple ->
      catch_compiler_errors (fun () -> f tuple))) in
  let register_string_primitive name f =
    register_primitive name (fun tuple ->
      f (Py.String.to_string (Py.Tuple.get tuple 0));
      Py.none) in
  register_string_primitive "require" require;
  register_primitive "compile" (fun tuple ->
    let code = Py.String.to_string (Py.Tuple.get tuple 0) in
    let module_name = module_name (count Ocaml_in_python_api.capsule_count) in
    let lexbuf = Lexing.from_string code in
    let structure = Ppxlib.Parse.implementation lexbuf in
    compile_and_load_structure ocaml_env module_name structure;
    import_ocaml_module_in_python ocaml_env Path.Map.empty module_name);
  register_string_primitive "add_dir" add_dir;
  register_string_primitive "loadfile" Dynlink.loadfile;
  register_primitive "debug" (fun _tuple ->
    debug := true;
    Py.none);
  register_primitive "__getattr__" (fun tuple ->
    let name = Py.String.to_string (Py.Tuple.get tuple 0) in
    import_ocaml_module_in_python ocaml_env Path.Map.empty name);
  let list = Py.Module.get ocaml "list" in
  Py.Object.set_attr_string list "_api_for_type"
    (Py.Callable.of_function_as_tuple (fun tuple ->
      find_collection_api list_api ocaml_env Path.Map.empty
        (Type.of_python (Py.Tuple.get tuple 0))));
  let array = Py.Module.get ocaml "array" in
  Py.Object.set_attr_string array "_api_for_type"
    (Py.Callable.of_function_as_tuple (fun tuple ->
      find_collection_api array_api ocaml_env Path.Map.empty
        (Type.of_python (Py.Tuple.get tuple 0))));
  let tuple = Py.Module.get ocaml "tuple" in
  Py.Object.set_attr_string tuple "_api_for_type"
    (Py.Callable.of_function_as_tuple (fun tuple ->
      find_tuple_api ocaml_env Path.Map.empty
        (Py.Tuple.to_list_map Type.of_python (Py.Tuple.get tuple 0))));
  let bytes_class = Py.Module.get ocaml "bytes" in
  let api_for_bytes = make_bytes_api ocaml_env Path.Map.empty in
  Py.Object.set_attr_string bytes_class "_api_for_type"
    (Py.Callable.of_function_as_tuple (fun _tuple -> api_for_bytes));
  let register_path path converter =
    let uid = uid_of_type_path ocaml_env path in
    Types.Uid.Tbl.add type_constr_converter_tbl uid converter in
  let register_lident lident converter =
    let uid = uid_of_type_lident ocaml_env lident in
    Types.Uid.Tbl.add type_constr_converter_tbl uid converter in
  register_path Predef.path_unit
    (fun _env _expansions _args -> {
      python_of_ocaml = Explicit (fun v -> [%expr [%e v]; Py.none]);
      ocaml_of_python = Explicit (fun v -> [%expr ignore [%e v]; ()]); });
  register_path Predef.path_int
    (fun _env _expansions _args -> {
      python_of_ocaml = Implicit [%expr Py.Int.of_int];
      ocaml_of_python = Implicit [%expr Py.Int.to_int] });
  register_path Predef.path_int64
    (fun _env _expansions _args -> {
      python_of_ocaml = Implicit [%expr Py.Int.of_int64];
      ocaml_of_python = Implicit [%expr Py.Int.to_int64] });
  register_path Predef.path_int32
    (fun _env _expansions _args -> {
      python_of_ocaml = Explicit (fun v -> [%expr
        Py.Int.of_int64 (Int64.of_int32 [%e v])]);
      ocaml_of_python = Explicit (fun v -> [%expr
        Int64.to_int32 (Py.Int.to_int64 [%e v])]); });
  register_path Predef.path_nativeint
    (fun _env _expansions _args -> {
      python_of_ocaml = Explicit (fun v -> [%expr
        Py.Int.of_int64 (Int64.of_nativeint [%e v])]);
      ocaml_of_python = Explicit (fun v -> [%expr
        Int64.to_nativeint (Py.Int.to_int64 [%e v])]); });
  register_path Predef.path_char
    (fun _env _expansions _args -> {
      python_of_ocaml = Implicit [%expr Ocaml_in_python_api.py_of_char];
      ocaml_of_python = Implicit [%expr Ocaml_in_python_api.char_of_py] });
  register_path Predef.path_string (fun _env _expansions _args -> {
    python_of_ocaml = Implicit [%expr Py.String.of_string];
    ocaml_of_python = Implicit [%expr Py.String.to_string] });
  register_path Predef.path_bool (fun _env _expansions _args -> {
    python_of_ocaml = Implicit [%expr Py.Bool.of_bool];
    ocaml_of_python = Implicit [%expr Py.Bool.to_bool] });
  register_path Predef.path_float (fun _env _expansions _args -> {
    python_of_ocaml = Implicit [%expr Py.Float.of_float];
    ocaml_of_python = Implicit [%expr Py.Float.to_float] });
  register_path Predef.path_array (fun ocaml_env expansions args ->
    match args with
    | [arg] -> value_converter_of_array ocaml_env expansions arg
    | _ -> assert false);
  register_path Predef.path_list (fun ocaml_env expansions args ->
    match args with
    | [arg] -> value_converter_of_list ocaml_env expansions arg
    | _ -> assert false);
  register_path Predef.path_bytes (fun _env _expansions _args ->
    value_converter_of_bytes);
  register_path Predef.path_option (fun ocaml_env expansions args ->
    let arg = match args with [arg] -> arg | _ -> assert false in
    let converter = Type.to_value_converter ocaml_env expansions arg in
    if Type.has_none ocaml_env arg then {
      python_of_ocaml = Explicit (fun v -> [%expr
        match [%e v] with
        | None -> Py.none
        | Some v ->
            let ocaml = Ocaml_in_python_api.get_root_python_module () in
            let some = Py.Module.get ocaml "Some" in
            Py.Callable.to_function_as_tuple some (Py.Tuple.singleton
              ([%e Ocaml_in_python_api.Function.to_expression converter.python_of_ocaml] v))]);
      ocaml_of_python = Explicit (fun v -> [%expr
        if v = Py.none then
          None
        else
          let ocaml = Ocaml_in_python_api.get_root_python_module () in
          let some = Py.Module.get ocaml "Some" in
          let v = [%e v] in
          let destructed =
            if Py.Object.is_instance v some then
              Py.Sequence.get v 0
            else
              v in
          Some ([%e Ocaml_in_python_api.Function.to_expression converter.ocaml_of_python] v)])}
    else {
      python_of_ocaml = Explicit (fun v -> [%expr
        match [%e v] with
        | None -> Py.none
        | Some v -> [%e Ocaml_in_python_api.Function.to_expression converter.python_of_ocaml] v]);
      ocaml_of_python = Explicit (fun v -> [%expr
        let v = [%e v] in
        if v = Py.none then
          None
        else
          Some ([%e Ocaml_in_python_api.Function.to_expression converter.ocaml_of_python] v)])});
  register_path Predef.path_extension_constructor
    (fun _env _expansions _args -> {
      python_of_ocaml = Implicit [%expr Ocaml_in_python_api.Extension_constructor.to_python];
      ocaml_of_python = Implicit [%expr Ocaml_in_python_api.Extension_constructor.of_python] });
  register_path Predef.path_floatarray
    (fun _env _expansions _args -> {
      python_of_ocaml = Implicit [%expr Py.Array.numpy];
      ocaml_of_python = Implicit [%expr Ocaml_in_python_api.get_floatarray] });
  let index = count type_count in
  let constructors = Ocaml_in_python_api.ExtensibleArray.create None 16 in
  Ocaml_in_python_api.exception_class := Py.Object.find_attr_string ocaml "exn";
  let class_var = LidentRef (Ldot (Lident "Ocaml_in_python_api", "exception_class")) in
  let name = "exn" in
  let type_info = {
      index;
      name;
      longident = Lident name;
      class_var;
      capsule_var = (fun _ -> failwith "Not implemented");
      type_declaration = Env.find_type Predef.path_exn ocaml_env;
      kind = Open constructors;
    } in
  Types.Uid.Tbl.add open_types_tbl type_info.type_declaration.type_uid {
      constructors; name; class_var; index };
  let exn_converter = make_type_converter type_info in
  exn_converter_ref := (fun ocaml_env expansions ->
    (exn_converter ocaml_env expansions []));
  Types.Uid.Tbl.add type_constr_converter_tbl
    type_info.type_declaration.type_uid exn_converter;
  ignore (prepare_compilation_immediate ());
  let python_module = [%expr Ocaml_in_python_api.get_root_python_module ()] in
  add_type_info ocaml_env Path.Map.empty python_module type_info;
  catch_compiler_errors (fun () ->
      perform_compilation ());
  let wrap_channel of_descr to_descr mode _env _expansions _args
      : Ocaml_in_python_api.value_converter = {
    python_of_ocaml = Explicit (fun v -> [%expr
      let fd = Ocaml_in_python_api.int_of_fd ([%e to_descr] [%e v]) in
      Py.Module.get_function_with_keywords (Py.Module.builtins ()) "open"
        [| Py.Int.of_int fd |] [
          "mode", [%e make_python_string mode];
          "buffering", Py.Bool.f;]]);
    ocaml_of_python = Explicit (fun v -> [%expr
      let fd = Py.Int.to_int (Py.Callable.to_function_as_tuple
        (Py.Object.find_attr_string [%e v] "fileno") Py.Tuple.empty) in
      [%e of_descr] (Ocaml_in_python_api.fd_of_int fd)])} in
  register_lident (Lident "in_channel")
    (wrap_channel [%expr Unix.in_channel_of_descr]
      [%expr Unix.descr_of_in_channel] "rb");
  register_lident (Lident "out_channel")
    (wrap_channel [%expr Unix.out_channel_of_descr]
      [%expr Unix.descr_of_out_channel] "ab");
  Type.value_converter_of_tuple := value_converter_of_tuple

let initialize_ocaml_env () =
  ignore (Warnings.parse_options false "-3-58"); (* deprecated, no-cmx-file *)
  Clflags.native_code := true;
  [%meta if Sys.ocaml_version >= "4.09.0" then
    [%e Ocaml_common.Compmisc.init_path ()]
  else
    [%e Ocaml_common.Compmisc.init_path false]];
  Ocaml_common.Compmisc.initial_env ()

let initialize_findlib () =
  Findlib.init ();
  add_dir (Findlib.package_directory "stdcompat");
  add_dir (Findlib.package_directory "pyml");
  begin match
    Findlib.package_directory "ocaml-in-python.api"
  with
  | dir -> add_dir dir
  | exception (Fl_package_base.No_such_package _) ->
      add_dir ("../../api/.ocaml_in_python_api.objs/byte/")
  end

let () =
  let ocaml_env = initialize_ocaml_env () in
  root_ocaml_env := Some ocaml_env;
  initialize_findlib ();
  initialize_python ocaml_env
