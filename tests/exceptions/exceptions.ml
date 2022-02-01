exception E of int

let catch f =
  try
    f ();
    assert false
  with E i -> i
