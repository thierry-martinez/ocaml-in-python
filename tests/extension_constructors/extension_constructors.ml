type t = ..

type t += A | B of int

let a = A

let b i = B i

let get_b b =
  match b with
  | B i -> i
  | _ -> assert false
