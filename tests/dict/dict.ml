type t = {
    mutable x : int;
    mutable y : int;
  }

let of_pair (x, y) = { x; y }

let to_pair {x; y} = (x, y)
