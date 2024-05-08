all: install run

install:
	opam install . --deps-only

run:
	dune build --release
	./bin/ray_tracing.exe

fmt:
	dune fmt