module ExtensibleArray = struct
  type 'a t = {
      mutable array : 'a array;
      mutable length : int;
      dummy : 'a;
    }

  let create dummy capacity =
    assert (capacity >= 1);
    {
      array = Array.make capacity dummy;
      length = 0;
      dummy;
    }

  let length ext =
    ext.length

  let get ext index =
    assert (index >= 0 && index < ext.length);
    ext.array.(index)

  let set ext index value =
    assert (index >= 0 && index < ext.length);
    ext.array.(index) <- value

  let push_f ext f =
    let index = ext.length in
    let new_length = succ index in
    ext.length <- new_length;
    let old_capacity = Array.length ext.array in
    let array =
      if new_length < old_capacity then
        ext.array
      else
        begin
          let new_capacity = old_capacity * 2 in
          let new_array = Array.make new_capacity ext.dummy in
          Array.blit ext.array 0 new_array 0 old_capacity;
          ext.array <- new_array;
          new_array
        end in
    array.(index) <- f index;
    index

  let push ext value =
    push_f ext (fun _ -> value)

  let to_list_map f ext =
    List.init ext.length (fun i -> f (get ext i))
end

let rec hash_path seed (p : Path.t) =
  match p with
  | Pident ident -> Hashtbl.seeded_hash seed (0, Ident.hash ident)
  | Pdot (p, s) -> Hashtbl.seeded_hash seed (1, hash_path seed p, s)
  | Papply (p, q) ->
      Hashtbl.seeded_hash seed (2, hash_path seed p, hash_path seed q)

let format_label (fmt : Format.formatter) (l : Ppxlib.arg_label) =
  match l with
  | Nolabel -> ()
  | Labelled s -> Format.fprintf fmt "~%s:" s
  | Optional s -> Format.fprintf fmt "?%s:" s

module Function = struct
  type t =
    | Implicit of Ppxlib.expression
    | Explicit of (Ppxlib.expression -> Ppxlib.expression)

  let apply (f : t) (e : Ppxlib.expression) =
    match f with
    | Implicit f -> [%expr [%e f] [%e e]]
    | Explicit f -> f e

  let to_expression (f : t) =
    match f with
    | Implicit f -> f
    | Explicit f -> [%expr fun v -> [%e f [%expr v]]]
end

type value_converter = {
    python_of_ocaml : Function.t;
    ocaml_of_python : Function.t;
  }

type converters_of_arity = {
    python_args : Ppxlib.expression;
    python_dict : Ppxlib.expression;
    ocaml_pats : (Ppxlib.arg_label * Ppxlib.pattern) list;
    ocaml_exps : (Ppxlib.arg_label * Ppxlib.expression) list;
  }

module Type = struct
  module Self = struct
    type t =
      | Any
      | Var of int
      | Arrow of param * t
      | Tuple of t list
      | Constr of Path.t * t list
    and param = {
        label : Ppxlib.arg_label;
        ty : t;
      }


    let rec hash seed t =
      match t with
      | Any -> Hashtbl.seeded_hash seed (-1)
      | Var x -> Hashtbl.seeded_hash seed (0, x)
      | Arrow ({ label; ty }, r) ->
          Hashtbl.seeded_hash seed (1, label, hash seed ty, hash seed r)
      | Tuple args ->
          Hashtbl.seeded_hash seed (2, List.map (hash seed) args)
      | Constr (p, args) ->
          Hashtbl.seeded_hash seed
            (3, hash_path seed p, List.map (hash seed) args)

    let rec equal t t' =
      match t, t' with
      | Any, Any -> true
      | Var x, Var y -> x = y
      | Arrow (p, r), Arrow (p', r') ->
          p.label = p'.label && equal p.ty p'.ty && equal r r'
      | Tuple args, Tuple args' ->
          List.equal equal args args'
      | Constr (p, args), Constr (p', args') ->
          Path.same p p' && List.equal equal args args'
      | _ -> false
  end

  include Self

  let rec subst f ty =
    match ty with
    | Any -> Any
    | Var index -> f index
    | Arrow ({ label; ty }, result) ->
        Arrow ({ label; ty = subst f ty }, subst f result)
    | Tuple list -> Tuple (List.map (subst f) list)
    | Constr (constr, args) -> Constr (constr, List.map (subst f) args)

  let map_param f param = { param with ty = f param.ty }

  type arity = {
      params : param list;
      result : t;
    }

  let map_arity f arity = {
      params = List.map (map_param f) arity.params;
      result = f arity.result;
    }

  let wrap, unwrap = Py.Capsule.make "ocaml.Type"

  let of_python (py_type : Py.Object.t) : t =
    let ocaml = Py.Import.import_module "ocaml" in
    if py_type = Py.Module.get (Py.Module.builtins ()) "object" then
      Any
    else if py_type = Py.Module.get ocaml "int" then
      Constr (Predef.path_int, [])
    else if py_type = Py.Module.get ocaml "float" then
      Constr (Predef.path_float, [])
    else if py_type = Py.Module.get ocaml "string" then
      Constr (Predef.path_string, [])
    else if py_type = Py.Module.get ocaml "bool" then
      Constr (Predef.path_bool, [])
    else
      unwrap (
        Py.Callable.to_function_as_tuple
          (Py.Object.find_attr_string py_type "_get_type")
          (Py.Tuple.singleton py_type))

  let rec arity_of_type (ty : t) : arity =
    match ty with
    | Arrow (param, result) ->
        let { params; result } = arity_of_type result in
        { params = param :: params; result }
    | _ ->
        { params = []; result = ty }

  let rec format (fmt : Format.formatter) (ty : t) =
    match ty with
    | Any -> Format.fprintf fmt "_"
    | Var i -> Format.fprintf fmt "'_%d" i
    | Arrow ({ label; ty }, r) ->
        Format.fprintf fmt "(%a%a -> %a)" format_label label format ty format r
    | Tuple args ->
        Format.fprintf fmt "(%a)"
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " * ")
            format) args
    | Constr (p, []) ->
        Format.fprintf fmt "%a" Path.print p
    | Constr (p, args) ->
        Format.fprintf fmt "(%a) %a"
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
            format) args Path.print p

  let to_string (ty : t) =
    Format.asprintf "%a" format ty

  let rec to_core_type (ty : t) : Ppxlib.Parsetree.core_type =
    match ty with
    | Any -> [%type: Py.Object.t]
    | Var  _ -> assert false
    | Arrow ({ label; ty }, r) ->
        Ppxlib.Ast_helper.Typ.arrow label (to_core_type ty) (to_core_type r)
    | Tuple args ->
        Ppxlib.Ast_helper.Typ.tuple (List.map to_core_type args)
    | Constr (p, args) ->
        let p = Untypeast.lident_of_path p in
        let args = List.map to_core_type args in
        Ppxlib.Ast_helper.Typ.constr (Metapp.mkloc p) args

  module Hashtbl = Hashtbl.MakeSeeded (Self)

  let to_value_converter_ref = ref (fun ?(name : string option) (_ : Env.t) (_ : Path.t Path.Map.t) (_ : t) :
    value_converter -> ignore name; failwith "Not yet available to_value_converter")

  let to_value_converter ?name env expansions ty =
    !to_value_converter_ref ?name env expansions ty

  let converters_of_arity_ref = ref (fun (_ : Env.t) (_ : Path.t Path.Map.t) (_ : arity) :
    converters_of_arity -> failwith "Not yet available converters_of_arity")

  let converters_of_arity env expansions arity =
    !converters_of_arity_ref env expansions arity

  let value_converter_of_function_ref = ref (fun ?(name : string option) (_ : Env.t) (_ : Path.t Path.Map.t) (_ : arity) :
    value_converter -> ignore name; failwith "Not yet available value_converter_of_function")

  let value_converter_of_function ?name env expansions arity =
    !value_converter_of_function_ref ?name env expansions arity

  let types : t ExtensibleArray.t =
    ExtensibleArray.create Any 16

  let type_table = Hashtbl.create 16

  let to_index ty =
    try
      Hashtbl.find type_table ty
    with Not_found ->
      let index = ExtensibleArray.push types ty in
      Hashtbl.add type_table ty index;
      index

  let of_index index =
    ExtensibleArray.get types index
end

module TypeList = struct
  module Self = struct
    type t = Type.t list

    let hash seed l =
      Hashtbl.seeded_hash seed (List.map (Type.hash seed) l)

    let equal l l' =
      List.equal Type.equal l l'
  end

  include Self

  module Hashtbl = Hashtbl.MakeSeeded (Self)
end

module Paths = struct
  type path_cell = {
      path : Path.t;
      class_ : Py.Object.t;
    }

  type index_cell = {
      index : int;
      class_ : Py.Object.t;
    }

  let dummy = { path = Predef.path_int; class_ = Py.null }

  let store : path_cell ExtensibleArray.t =
    ExtensibleArray.create dummy 16

  let converted_map_ref = ref Path.Map.empty

  let find_opt path =
    Path.Map.find_opt path !converted_map_ref

  let get index =
    ExtensibleArray.get store index

  let register path class_ =
    let converted_map = !converted_map_ref in
    let index = ExtensibleArray.push store { path; class_ } in
    converted_map_ref := Path.Map.add path { index; class_ } converted_map;
    index
end

type variable_index = {
    module_index : int;
    local_index : int;
  }

let array_capsules : variable_index Type.Hashtbl.t = Type.Hashtbl.create 16

let array_api : Py.Object.t Type.Hashtbl.t = Type.Hashtbl.create 16

let list_capsules : variable_index Type.Hashtbl.t = Type.Hashtbl.create 16

let list_api : Py.Object.t Type.Hashtbl.t = Type.Hashtbl.create 16

let tuple_capsules : variable_index TypeList.Hashtbl.t =
  TypeList.Hashtbl.create 16

let tuple_api : Py.Object.t TypeList.Hashtbl.t = TypeList.Hashtbl.create 16

module IntHashtbl = Hashtbl.MakeSeeded (struct
  type t = int

  let equal = Int.equal

  let hash = Hashtbl.seeded_hash
end)

type 'a api = {
    api : 'a;
    make : Py.Object.t -> Py.Object.t;
  }

type 'a type_def_info = {
    make_capsule : TypeList.t -> unit;
    make_api : TypeList.t -> unit;
    api_table : 'a api TypeList.Hashtbl.t;
  }

let type_def_table : Py.Object.t type_def_info IntHashtbl.t = IntHashtbl.create 16

let api_for_type type_def_info tuple =
  let types = Py.Tuple.get tuple 0 in
  let type_list =
    try
      Py.List.to_list_map Type.of_python types
    with _ ->
      [Type.of_python types] in
  let api =
    try
      TypeList.Hashtbl.find type_def_info.api_table type_list
    with Not_found ->
      type_def_info.make_capsule type_list;
      type_def_info.make_api type_list;
      try
        TypeList.Hashtbl.find type_def_info.api_table type_list
      with Not_found ->
        failwith "api_for_type" in
  api.api

let variant_table : Py.Object.t array type_def_info IntHashtbl.t = IntHashtbl.create 16

module OpenType = struct
  let table : Py.Object.t array type_def_info IntHashtbl.t = IntHashtbl.create 16
end

let capsule_count = ref 0

let get_root_python_module () =
  Py.Import.import_module "ocaml"

external fd_of_int : int -> Unix.file_descr = "%identity"

external int_of_fd : Unix.file_descr -> int = "%identity"

let py_of_char c =
  Py.String.of_string (String.make 1 c)

let char_of_py obj =
  let s = Py.String.to_string obj in
  if String.length s <> 1 then
    raise (Py.Err (TypeError,
      Printf.sprintf "char expected but \"%s\" given" s));
  s.[0]

let bytes_capsule : (bytes -> Py.Object.t) * (Py.Object.t -> bytes) =
  Py.Capsule.make "ocaml.bytes"

let raise_index_out_of_bounds ~index ~length =
  raise (Py.Err (IndexError, Printf.sprintf "Index %d out of bounds 0<=.<%d"
    index length))

type generic_python_function =
  args_tuple:Py.Object.t -> keywords_dict:Py.Object.t -> Py.Object.t

module PolymorphicFunction = struct
  type t = {
      make : TypeList.t -> generic_python_function;
      table : generic_python_function TypeList.Hashtbl.t;
    }

  let table : t option ExtensibleArray.t =
    ExtensibleArray.create None 16

  let get index =
    Option.get (ExtensibleArray.get table index)

  let push f =
    ExtensibleArray.push_f table (fun index -> Some (f index))
end

let get_floatarray obj =
  try Py.Array.numpy_get_array obj
  with Not_found ->
    let len = Py.Sequence.length obj in
    let result = Array.Floatarray.create len in
    for i = 0 to len - 1 do
      Array.Floatarray.set result i
        (Py.Float.to_float (Py.Sequence.get_item obj i));
    done;
    result

module Extension_constructor = struct
  let (to_python, of_python) : (extension_constructor -> Py.Object.t) * (Py.Object.t -> extension_constructor) =
    Py.Capsule.make "extension_constructor"
end

let exception_class = ref Py.none

let pending_module_table : Py.Object.t Lazy.t Path.Map.t ref =
  ref Path.Map.empty

let pending_modules : Py.Object.t Lazy.t ExtensibleArray.t =
  ExtensibleArray.create (lazy (failwith "not yet available")) 16
