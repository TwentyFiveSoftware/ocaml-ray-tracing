all: install run

install:
	opam install . --deps-only

run:
	dune build
	dune exec ray_tracing
