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

Documentation is published at https://aestheticintegration.github.io/socket.io-client-ocaml.

Also see `examples/engine.io/client.ml` and `examples/socket.io/client.ml` for
usage examples.

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

make example-socketio-server
make example-socketio-client
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

## TODO

- Engine.io: Sending/receiving binary messages is largely un-tested.
- Socket.io: [Auto-reconnection][3] (a simple back-off strategy for the initial
  connection is currently implemented in the Engine.io client.)
- Socket.io: [Binary support][4]

[3]: https://github.com/socketio/socket.io#auto-reconnection-support
[4]: https://github.com/socketio/socket.io#binary-support
