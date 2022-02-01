import ocaml
ocaml.add_dir("../../api/.ocaml_in_python_api.objs/byte/")
ocaml.add_dir(".extension_constructors.objs/byte/")
ocaml.loadfile("extension_constructors.cmxs")

from ocaml import Extension_constructors

match Extension_constructors.a:
    case Extension_constructors.B(_):
        assert(False)
    case Extension_constructors.A():
        pass
    case _:
        assert(False)

match Extension_constructors.b(2):
    case Extension_constructors.B(1):
        assert(False)
    case Extension_constructors.B(2):
        pass
    case _:
        assert(False)

assert(Extension_constructors.get_b(Extension_constructors.B(1)) == 1)
