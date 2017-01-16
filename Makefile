.PHONY: build
build:
	ocaml pkg/pkg.ml build

.PHONY: test
test:
	ocaml pkg/pkg.ml test

dev-setup:
	opam pin add . --no-action --yes
	opam install ocaml-engineio-client --deps-only
