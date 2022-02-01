import ocaml
ocaml.require("parmap")
from ocaml import Parmap
print(Parmap.parmap(
  (lambda x : x + 1), Parmap.A([1, 2, 3]), ncores=2,
  type={ "a": int, "b": int }))
