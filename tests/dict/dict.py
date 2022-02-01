import ocaml
ocaml.add_dir("../../api/.ocaml_in_python_api.objs/byte/")
ocaml.add_dir(".dict.objs/byte/")
ocaml.Dynlink.loadfile("dict.cmxs")
from ocaml import Dict
p = Dict.of_pair((1, 2))
assert p.x == 1 and p.y == 2
p.x = 3
assert tuple(Dict.to_pair(p)) == (3, 2)
assert tuple(Dict.to_pair({ "x": 4, "y": 5 })) == (4, 5)
