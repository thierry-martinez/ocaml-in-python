module CompleteBinaryTree = struct
  type 'a t =
    | Leaf of 'a
    | Node of ('a * 'a) t

  let rec make : 'a . int -> 'a -> 'a t = fun depth value ->
    if depth > 0 then
      Node (make (depth - 1) (value, value))
    else
      Leaf value

  let rec iter : 'a . ('a -> unit) -> 'a t -> unit = fun f tree ->
    match tree with
    | Leaf value -> f value
    | Node tree ->
        iter (fun (a, b) -> f a; f b) tree
end
