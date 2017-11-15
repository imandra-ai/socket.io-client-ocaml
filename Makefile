.PHONY: build
build:
	jbuilder build

.PHONY: test
test:
	jbuilder runtest

.PHONY: clean
clean:
	jbuilder clean

.PHONY: doc
doc:
	jbuilder build @doc

.PHONY: examples
examples: _build/default/examples/engine.io/client.exe _build/default/examples/socket.io/client.exe
	cd examples/engine.io && npm install
	cd examples/socket.io && npm install

_build/default/examples/engine.io/client.exe: examples/engine.io/client.ml
	jbuilder build examples/engine.io/client.exe

_build/default/examples/socket.io/client.exe: examples/socket.io/client.ml
	jbuilder build examples/socket.io/client.exe

.PHONY: run-example-engineio-server
run-example-engineio-server:
	cd examples/engine.io && DEBUG=engine* node index.js

.PHONY: run-example-engineio-client
run-example-engineio-client: _build/default/examples/engine.io/client.exe
	_build/default/examples/engine.io/client.exe

.PHONY: run-example-socketio-server
run-example-socketio-server:
	cd examples/socket.io && DEBUG=engine*,socket.io* node index.js

.PHONY: run-example-socketio-client
run-example-socketio-client: _build/default/examples/socket.io/client.exe
	_build/default/examples/socket.io/client.exe
