(lang dune 2.9)

(generate_opam_files true)

(name ocaml-in-python)
(license "BSD-2-Clause")
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://github.com/thierry-martinez/ocaml-in-python"))
(homepage "https://github.com/thierry-martinez/ocaml-in-python")
(bug_reports "https://github.com/thierry-martinez/ocaml-in-python")
(documentation "https://github.com/thierry-martinez/ocaml-in-python")
(version "0.1.0")

(package
 (name ocaml-in-python)
 (synopsis "Effortless Python bindings for OCaml modules")
 (description "\
Effortless Python bindings for OCaml modules
")
 (depends
  (ocaml (>= 4.13.0))
  (ppxlib (>= 0.23.0))
  (metapp (>= 0.4.3))
  (metaquot (>= 0.5.1))
  (pyml (>= 20220325))
  conf-python-3
  (parmap :with-test)))