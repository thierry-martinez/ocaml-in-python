import ocaml
ocaml.add_dir("../../api/.ocaml_in_python_api.objs/byte/")
ocaml.add_dir(".extension_constructors.objs/byte/")
ocaml.loadfile("extension_constructors.cmxs")

from ocaml import Extension_constructors

assert(Extension_constructors.get_b(Extension_constructors.B(1)) == 1)

import sys

if sys.hexversion >= 0x03100000:
    import extension_constructors_3_10
