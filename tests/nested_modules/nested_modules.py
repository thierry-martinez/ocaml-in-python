import ocaml
ocaml.add_dir("../../api/.ocaml_in_python_api.objs/byte/")
ocaml.add_dir(".nested_modules.objs/byte/")
ocaml.Dynlink.loadfile("nested_modules.cmxs")
from ocaml import Nested_modules
print(Nested_modules.A.c)
Nested_modules.A.f(Nested_modules.A.c)
