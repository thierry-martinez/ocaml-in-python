module A = struct
  module B = struct
    type t = C
  end

  let c = B.C

  let f x =
    match x with
    | B.C -> ()
end
