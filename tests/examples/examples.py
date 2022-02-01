import ocaml

import ocaml

m = ocaml.compile(r'''
  let hello x = Printf.printf "Hello, %s!\n%!" x

  type 'a tree = Node of { label : 'a; children : 'a tree list }

  let rec height (Node { label = _; children }) =
    1 + List.fold_left (fun accu tree -> max accu (height tree)) 0 children
''')

m.hello("world")
# => output: Hello, world!

print(m.height(
  m.Node(label=1, children=[m.Node(label=2, children=[])])))
# => output: 2

ocaml.require("parmap")
from ocaml import Parmap
print(Parmap.parmap(
  (lambda x : x + 1), Parmap.A([1, 2, 3]), ncores=2,
  type={ "a": int, "b": int }))
