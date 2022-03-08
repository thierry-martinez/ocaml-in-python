import ocaml
ocaml.debug()
assert(ocaml.Result.get_ok(ocaml.Result.Ok(True)) == True)
