(*
let test f (l : Py.Object.t Parmap.sequence) =
  let l = Parmap.parmap f l in
  prerr_endline (Py.Object.to_string (Py.List.of_list l))
*)

let test () =
  let m = Py.Import.exec_code_module_from_string ~name:"test" {|
def f(x):
  return x + 1
|} in
  let f = Py.Module.get_function m "f" in
  let l = Parmap.parmap (fun item -> f [| item |])
    (A (Array.of_list (List.map Py.Int.of_int [1; 2; 3]))) in
  prerr_endline (Py.Object.to_string (Py.List.of_list l));
  let m = Py.Import.exec_code_module_from_string ~name:"test" {|
def f(x):
  return x + 1
|} in
  let f = Py.Module.get_function m "f" in
  let l = Parmap.parmap (fun item -> f [| item |])
    (A (Array.of_list (List.map Py.Int.of_int [1; 2; 3]))) in
  prerr_endline (Py.Object.to_string (Py.List.of_list l))
