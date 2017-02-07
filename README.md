ocaml-engineio-client
=====================

Engine.io client for OCaml.

See https://github.com/socketio/engine.io-protocol.

## Install

With `opam`:

```
opam pin add engineio-client https://github.com/AestheticIntegration/ocaml-engineio-client.git
```

## Usage

See `examples/engineio/client.ml` and https://aestheticintegration.github.io/ocaml-engineio-client.

## Development

Requires `opam`.

```
make dev-setup
make build
make test
```

## Running the examples

Requires `node` and `npm`.

```
make examples-setup
make example-engineio-server
make example-engineio-client
```

## Documentation

To generate the documenation (requires `topkg-care` from `opam`):

```
opam install topkg-care
make doc
```

To publish the generated documentation:

```
git checkout gh-pages
rm *.html *.stamp *.css
mv _build/doc/api.docdir/* .
rm -rf _build
git add .
git commit -m "Re-generate docs."
git push origin gh-pages
```
