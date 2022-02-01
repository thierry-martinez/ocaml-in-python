# Effortless Python bindings for OCaml modules

This library exposes any OCaml module as a Python module, generating
bindings on the fly.

In the following example, we will call the library
[`parmap`](https://github.com/rdicosmo/parmap) from Python.

```
import ocaml

ocaml.require("parmap")

from ocaml import Parmap

print(Parmap.parmap(
  (lambda x : x + 1), Parmap.A([1, 2, 3]), ncores=2,
  type={ "a": int, "b": int }))
# => output: [2, 3, 4]
```

This example uses `ocaml.require`, which relies on the fact
that `parmap` is available *via*
[`ocamlfind`](https://github.com/ocaml/ocamlfind).
`Parmap.A` is one of the two constructors of the type `Parmap.sequence`.
`Parmap.parmap` is polymorphic with two type parameters `'a`
(the type of the inputs) and `'b` (the type of the outputs).
The `type=` optional argument allows the caller to specify the types,
otherwise `Py.Object.t` is used by default (and `Parmap.parmap` cannot
serialize the abstract types).
