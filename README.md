socket.io-client-ocaml
======================

[Engine.io][1] and [Socket.io][2] clients for OCaml.

[1]: https://github.com/socketio/engine.io-protocol
[2]: https://github.com/socketio/socket.io-protocol

## Install

With `opam`:

```
opam pin add socketio-client https://github.com/AestheticIntegration/socket.io-client-ocaml.git
```

## Usage

Documentation is published at https://aestheticintegration.github.io/socket.io-client-ocaml/doc.

Also see `examples/engine.io/client.ml` and `examples/socket.io/client.ml` for
usage examples.

## Development

With `opam2`:

```
opam switch create ./
eval $(opam env)
opam install . --deps-only --with-test
make build
make test
```

## Running the examples

Requires `node` and `npm`.

```
make examples

make run-example-engineio-server
make run-example-engineio-client

make run-example-socketio-server
make run-example-socketio-client
```

## Documentation

To generate the documenation (requires `topkg-care` from `opam`):

```
opam install topkg-care
make doc
```

To generate and publish documentation:

```
topkg distrib
topkg publish doc
```

## TODO

- Engine.io: Sending/receiving binary messages is largely un-tested.
- Socket.io: [Auto-reconnection][3] (a simple back-off strategy for the initial
  connection is currently implemented in the Engine.io client.)
- Socket.io: [Binary support][4]

[3]: https://github.com/socketio/socket.io#auto-reconnection-support
[4]: https://github.com/socketio/socket.io#binary-support
