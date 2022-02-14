# Effortless Python bindings for OCaml modules

This library exposes any OCaml module as a Python module, generating
bindings on the fly.

The OCaml module can either be compiled on the fly or loaded dynamically.

- In the following example, a module is compiled on the fly from Python.

```python
import ocaml

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
# => output: Node {label="a";children=[Node {label="b";children=[Node {label="c";children=[]}]}]}

try:
    print(m.of_list([]))
except ocaml.Stdlib.Invalid_argument as e:
    print(e)
    # => output: Stdlib.Invalid_argument("of_list")
```

It is worth noticing that there is no need for type annotations:
bindings are generated with respect to the interface obtained
by type inference.

- In the following example, we call the library
[`parmap`](https://github.com/rdicosmo/parmap) from Python.

```python
import ocaml

ocaml.require("parmap")

from ocaml import Parmap

print(Parmap.parmap(
  (lambda x : x + 1), Parmap.A([1, 2, 3]), ncores=2))
# => output: [2, 3, 4]
```

The function `ocaml.require` uses
[`ocamlfind`](https://github.com/ocaml/ocamlfind) to load `parmap`.
Bindings are generated as soon as `ocaml.Parmap` is accessed
(in the example, at line `from ocaml import Parmap`).
`Parmap.A` is one of the two constructors of the type `Parmap.sequence`.

