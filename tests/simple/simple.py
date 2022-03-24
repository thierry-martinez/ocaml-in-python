import ocaml
ocaml.add_dir("../../api/.ocaml_in_python_api.objs/byte/")
ocaml.add_dir(".simple.objs/byte/")
ocaml.loadfile("simple.cmxs")
from ocaml import Simple
assert(not(isinstance(Simple.a, Simple.B)))
assert(isinstance(Simple.a, Simple.A))
assert(not(isinstance(Simple.b(2), Simple.A)))
assert(isinstance(Simple.b(2), Simple.B))
assert(Simple.b(2)[0] == 2)

import sys

if sys.hexversion >= 0x03100000:
    import simple_3_10
