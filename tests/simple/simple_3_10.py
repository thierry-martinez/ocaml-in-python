import ocaml
ocaml.add_dir("../../api/.ocaml_in_python_api.objs/byte/")
ocaml.add_dir(".simple.objs/byte/")
ocaml.loadfile("simple.cmxs")
from ocaml import Simple
match Simple.a:
    case Simple.B(1):
        assert(False)
    case Simple.A():
        pass
    case _:
        assert(False)

match Simple.b(2):
    case Simple.B(1):
        assert(False)
    case Simple.B(2):
        pass
    case _:
        assert(False)
