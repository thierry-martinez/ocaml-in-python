let parmap f seq =
  prerr_endline "HERE parmap";
  Parmap.parmap f seq
