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

See `examples/engineio/client.ml`

## Develop

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

Requires `topkg-care` from `opam`.

```
opam install topkg-care
make doc
```
