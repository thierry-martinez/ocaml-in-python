# Effortless Python bindings for OCaml modules

This library exposes all OCaml modules as Python modules, generating
bindings on the fly.

## Requirements

- `OCaml` >= 4.13

- `Python` >= 3.7 (and >= 3.10 for pattern-matching support)

## Setup

The package can be installed via `opam`:

- `opam install ocaml-in-python` installs the latest release,

- `opam pin add -k path . && opam install ocaml-in-python`
  executed in a clone of this repository installs the latest development version.

Once installed via `opam`, the package should be registered in the Python environment.
There are two options:

- either you register the package with `pip` using the following command:
```bash
pip install --editable "`opam var ocaml-in-python:lib`"
```

- or you add the following definition to your environment:
```bash
export PYTHONPATH="`opam var share`/python/:$PYTHONPATH"
```

## Examples

### Standard library

A very simple mean to test that the bindings are working properly is to invoke
the OCaml standard library from Python.

```python
import ocaml
print(ocaml.List.map((lambda x : x + 1), [1, 2, 3]))
# => output: [2;3;4]
```

In the following example, we invoke the `ref` function from the OCaml
standard library to create a value of type `int ref` (a mutable
reference to an integer), and the following commands show that the
reference can be mutated from Python (a reference is a record with a
mutable field `contents`) and from OCaml (here by invoking the OCaml
function `incr`).

```python
>>> x = ocaml.ref(1, type=int)
>>> x
{'contents':1}
>>> x.contents = 2
>>> x
{'contents':2}
>>> ocaml.incr(x)
>>> x
{'contents':3}
```

### OCaml module compiled on the fly

In the following example, we compile a module on the fly from Python.

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
except ocaml.Invalid_argument as e:
    print(e)
    # => output: Stdlib.Invalid_argument("of_list")
```

It is worth noticing that there is no need for type annotations:
bindings are generated with respect to the interface obtained
by type inference.

### Requiring a library with `findlib`

In the following example, we call the OCaml library
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

## Conversion rules

The generation of bindings is driven by the types exposed by the
compiled module interfaces (`*.cmi` files): relying on the `*.cmi`
files allows the bindings to cover most of the OCaml definitions
(there are some limitations though, see below) and to use the inferred
types for modules whose interface is not explicitly specified by a
`.mli` file.

### Built-in types

The following conversions are defined for built-in types:

- OCaml `int`, `nativeint` `int32`, `int64` are mapped to Python `int`;

```python
import ocaml

ocaml.print_endline(ocaml.string_of_int(42))
# => output: 42
print(ocaml.int_of_string("5") + 1)
# => output: 6
```

- OCaml `string` is mapped to Python `str`

```python
import ocaml

ocaml.print_endline("Hello, World!")
# => output: Hello, World!
print(ocaml.String.make(3, "a") + "b")
# => output: aaab
```

- OCaml `char` is mapped to Python `str` with a single character

```python
import ocaml

print(ocaml.int_of_char("a"))
# => output: 97
print(ocaml.char_of_int(65))
# => output: A
```

- OCaml `bool` is mapped to Python `bool` (beware of different case convention:
OCaml values `false` and `true` are mapped to Python values
`False` and `True` respectively)

```python
import ocaml

print(ocaml.Sys.interactive.contents)
# => output: False
print(ocaml.string_of_bool(True))
# => output: true
```

- OCaml `float` is mapped to Python `float`, and functions taking
floats as arguments can take benefit from the Python automatic
coercion from `int` to `float`

```python
import ocaml

print(ocaml.float_of_int(1))
# => output: 1.0
print(ocaml.cos(0))
# => output: 1.0
```

- OCaml `array` is mapped to a dedicated class `ocaml.array`, which
  supports indexing, enumeration, pattern-matching (with Python >= 3.10)
  and in-place modification. When an OCaml array is converted to a Python
  object, the elements are converted on demand.
  There is an implicit coercion to array from all Python iterable types
  such as Python lists (but in-place modification is lost).

```python
import ocaml

arr = ocaml.Array.make(3, 0)
arr[1] = 1
print(ocaml.Array.fold_left((lambda x,y : x + y), 0, arr))
# => output: 1
ocaml.Array.sort(ocaml.compare, arr)
print(list(arr))
# => output: [0, 0, 1]
print(ocaml.Array.map((lambda x: x + 1), range(0, 4)))
# => output: [|1;2;3;4|]

# With Python 3.10:
match arr:
  case [0, 0, 1]:
    print("Here")
# => output: Here
```

  It is worth noticing that `Array.make` is a polymorphic function
  parameterized in the type of the elements of the constructed array,
  and by default the type parameter for polymorphic function with
  `ocaml-in-python` is `Py.Object.t`, the type of all Python objects.
  As such, the cells of the array `arr` defined above can contain any
  Python objects, not only integers.

```python
arr[0] = "Test"
print(arr)
# => output: [|"Test";0;1|]
```

  We can create an array with a specific types for cells by
  expliciting the type parameter of `Array.make`, by using the keyword
  parameter `type`.
  
```python
arr = ocaml.Array.make(3, 0, type=int)
arr[0] = "Test"
# TypeError: 'str' object cannot be interpreted as an integer
```

- OCaml `list` is mapped to a dedicated class `ocaml.list`, which
  supports indexing, enumeration and pattern-matching (with Python >= 3.10).
  When an OCaml list is converted to a Python
  object, the elements are converted on demand.
  There is an implicit coercion to list from all Python iterable types
  such as Python lists.

- OCaml `bytes` is mapped to a dedicated class `ocaml.bytes`, which
  behaves as a mutable collection of characters.

- OCaml `option` is mapped to a dedicated class `ocaml.option`, only
  for values of the form `Some x` where the type of `x` allows the
  value `None`. If the type of `x` does not contain a value `None`,
  the OCaml value `Some x` is mapped directly to the conversion of `x`.
  Conversely, the value `Some x` can be constructed
  with `ocaml.Some(x)`.
  The OCaml value `None` is mapped to the Python value `None`.
  
```python
print(ocaml.List.find_opt((lambda x : x > 1), [0,1], type=int))
# => output: None
print(ocaml.List.find_opt((lambda x : x > 1), [0,1,2], type=int))
# => output: 2
print(ocaml.List.find_opt((lambda x : x > 1), [0,1,2]))
# => output: Some(2)
```

  In the last call to `find_opt`, the default type parameter is `Py.Object.t`
  which contains the value `None`.

- OCaml `exn` is mapped to a dedicated class `ocaml.exn`, which is a
  sub-class of Python `Error` class, and exceptions are converted as
  other extension constructors: each exception is a sub-class of `ocaml.exn`,
  and values can be indexed (if the exception constructor takes parameters),
  accessed by field name (for inline records) and supports
  pattern-matching (with Python >= 3.10).
  There is an implicit coercion from other sub-classes of Python `Error` class
  to `Py.E`, the OCaml exception defined in `pyml` for Python exceptions.
  If an exception is raised between OCaml and Python code, the exception is
  converted and raised from one side to the other.

```python
try:
    ocaml.failwith("Test")
except ocaml.Failure as e:
    print(e[0])
# => output: Test
```

- OCaml `in_channel` and `out_channel` are mapped to `FileIO` objects.
  In the following example, the OCaml function `open_out` is used to create
  a new file `test`, and the string `Hello` is written in this file through
  the Python method `write`. Then, the file `test` is opened for reading
  with the Python built-in `open`, and the channel is read with the OCaml
  function `really_input_string`.

```python
with ocaml.open_out("test") as f:
   f.write(b"Hello")
with open("test", "r") as f:
   print(ocaml.really_input_string(f, 5))
# => ouput: Hello
```

- OCaml `floatarray` is mapped to a dedicated class `ocamlarray`,
  which derives from `numpy` array (`numpy` is required for the
  support of `floatarray`).

### Type constructors

- OCaml functions of type `'t_1 -> ... -> 't_n -> 'r`
  are mapped to Python callable objects with `n` arguments.
  Labelled arguments are mapped to mandatory keyword arguments,
  and optional arguments are mapped to optional keyword arguments.
  For polymorphic functions, the type parameters are assumed to be
  `Py.Object.t`, except if there is a keyword argument `type`:
  the associated value can either be a single type if there is
  a single type parameter, or a tuple of types giving the type
  parameters in the order of their apparition in the function signature,
  or a dictionary whose keys are the names of the type parameters
  (e.g., `"a"` for `'a`).

- OCaml tuples of type `'t_1 * ... * 't_n`
  are mapped to OCaml tuples with `n` components.
  
### Type definitions

Each OCaml type definition introduces a new Python class, except for
type aliases, that are exposed as other names for the same class.

Records are accessible by field name or index (in the order of the
field declarations), and the values of the fields are converted on
demand. Mutable fields can be set in Python. In particular, the `ref` 
type defined in the OCaml standard library is mapped to the Python
class `ocaml.ref` with a mutable field `content`.
Records support pattern-matching (with Python >= 3.10).
There is an implicit coercion from Python dictionaries with matching
field names.

For variants, there is a sub-class by constructor, which behaves
either as a tuple or as a record.
The values of the arguments are converted on demand.
Variants support pattern-matching (with Python >= 3.10).

### Sub-module definitions

Sub-modules are mapped to classes, which are constructed on demand.
For instance, the module `Array.Floatarray` is exposed as
`ocaml.Array.Floatarray`, and, in particular, the function
`Array.Floatarray.create` is available as
`ocaml.Array.Floatarray.create`.

## Limitations

The following traits of the OCaml type system are not supported (yet):

- records with polymorphic fields,
- polymorphic variants,
- objects,
- functors,
- first class modules.
