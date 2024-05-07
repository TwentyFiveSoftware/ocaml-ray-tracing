(lang dune 3.15)

(name ray_tracing)

(generate_opam_files true)

(source
 (github TwentyFiveSoftware/ocaml-ray-tracing))

(authors "TwentyFiveSoftware")
(maintainers "TwentyFiveSoftware")

(package
 (name ray_tracing)
 (synopsis "OCaml Ray Tracing")
 (description "OCaml implementation of Peter Shirley's Ray Tracing in One Weekend book")
 (depends ocaml dune imagelib))
