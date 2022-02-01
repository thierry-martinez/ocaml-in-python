import ocaml
ocaml.add_dir("../../api/.ocaml_in_python_api.objs/byte/")
ocaml.add_dir(".exceptions.objs/byte/")
ocaml.loadfile("exceptions.cmxs")
try:
    ocaml.Stdlib.failwith("Test")
    assert(False)
except ocaml.Stdlib.Failure as e:
    assert(e[0] == "Test")
def f():
    raise ocaml.Exceptions.E(1)
assert(ocaml.Exceptions.catch(f) == 1)
