#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let examples =
  Conf.key
    "build-examples"
    Conf.bool
    ~doc:"Build the example programs"
    ~absent:false

let () =
  Pkg.describe "engineio-client" @@ fun c ->
  let examples = Conf.value c examples in
  Ok [ Pkg.mllib "src/engineio_client.mllib"
     ; Pkg.mllib "src/socketio_client.mllib"
     ; Pkg.test ~dir:"src" "test/main"
     ; Pkg.bin ~cond:examples "examples/engineio/client"
     ; Pkg.bin ~cond:examples "examples/socketio/client"
     ]
