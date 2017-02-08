(* Copyright 2017 Aesthetic Integration, Ltd.                               *)
(*                                                                          *)
(* Author: Matt Bray (matt@aestheticintegration.com)                        *)
(*                                                                          *)
(* Licensed under the Apache License, Version 2.0 (the "License");          *)
(* you may not use this file except in compliance with the License.         *)
(* You may obtain a copy of the License at                                  *)
(*                                                                          *)
(*     http://www.apache.org/licenses/LICENSE-2.0                           *)
(*                                                                          *)
(* Unless required by applicable law or agreed to in writing, software      *)
(* distributed under the License is distributed on an "AS IS" BASIS,        *)
(* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *)
(* See the License for the specific language governing permissions and      *)
(* limitations under the License.                                           *)
(*                                                                          *)

open OUnit2
open Engineio_client

let assert_packets_equal packets1 packets2 =
  assert_equal
    ~printer:(fun packets ->
        String.concat "; "
          (List.map Packet.string_of_t packets))
    packets1
    packets2

let test_decode_payload_as_binary test_ctxt =
  assert_packets_equal
    [ (Packet.OPEN, Packet.P_None) ]
    (Parser.decode_payload_as_binary "\000\001\2550")

let test_encode_payload test_ctxt =
  assert_equal
    ~printer:(fun s -> s)
    "\000\006\2552probe"
    (Parser.encode_payload [(Packet.PING, Packet.P_String "probe")])

let suite =
  "Parser" >:::
  [ "decode_payload_as_binary" >:: test_decode_payload_as_binary
  ; "encode_payload" >:: test_encode_payload
  ]

let () =
  run_test_tt_main suite
