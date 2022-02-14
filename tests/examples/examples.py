import ocaml
from ocaml import Stdlib
#ocaml.add_dir("../../api/.ocaml_in_python_api.objs/byte/")
#ocaml.add_dir(".examples.objs/byte/")
#try:
#    ocaml.Dynlink.loadfile("examples.cmxs")
#except ocaml.Dynlink.Error as e:
#    print(ocaml.Dynlink.error_message(e[0]))

m = ocaml.compile(r'''
  let hello x = Printf.printf "Hello, %s!\n%!" x

  type 'a tree = Node of { label : 'a; children : 'a tree list }

  let rec height (Node { label = _; children }) =
    1 + List.fold_left (fun accu tree -> max accu (height tree)) 0 children

  let rec of_list nodes =
    match nodes with
    | [] -> invalid_arg "of_list"
    | [last] -> Node { label = last; children = [] }
    | hd :: tl -> Node { label = hd; children = [of_list tl] }
''')

m.hello("world")
# => output: Hello, world!

print(m.height(
  m.Node(label=1, children=[m.Node(label=2, children=[])])))
# => output: 2

print(m.of_list(["a", "b", "c"]))
# => output: Node {label=a;children=[Node {label=b;children=[Node {label=c;children=[]}]}]}

try:
    print(m.of_list([]))
except ocaml.Stdlib.Invalid_argument as e:
    print(e)
    # => output: Stdlib.Invalid_argument("of_list")

ocaml.require("parmap")
from ocaml import Parmap
print(Parmap.parmap((lambda x : x + 1), Parmap.A([1, 2, 3]), ncores=2))
