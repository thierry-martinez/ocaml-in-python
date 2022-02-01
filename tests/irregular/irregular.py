import ocaml
ocaml.add_dir("../../api/.ocaml_in_python_api.objs/byte/")
ocaml.add_dir(".irregular.objs/byte/")
ocaml.Dynlink.loadfile("irregular.cmxs")
from ocaml import Irregular
tree = Irregular.CompleteBinaryTree.make(3, "a")
accu = []
Irregular.CompleteBinaryTree.iter((lambda x: accu.append(x)), tree)
assert len(accu) == 8
tree = Irregular.CompleteBinaryTree.Node(Irregular.CompleteBinaryTree.Node(Irregular.CompleteBinaryTree.Leaf(((1, 2), (3, 4)))))
accu = []
Irregular.CompleteBinaryTree.iter((lambda x: accu.append(x)), tree)
assert accu == [1, 2, 3, 4]
