#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "ocaml-engineio-client" @@ fun c ->
  Ok [ Pkg.mllib "src/engineio_client.mllib"
     ; Pkg.test ~dir:"src" "test/main"
     ]
