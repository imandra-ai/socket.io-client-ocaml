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

let () =
  Lwt_log.default :=
    Lwt_log.channel
      ~template:"$(date).$(milliseconds) [$(section)] $(level): $(message)"
      ~close_mode:`Close
      ~channel:Lwt_io.stdout ()

let assert_string_equal s1 s2 =
  assert_equal
    ~printer:(fun s -> s)
    s1
    s2

let engineio_parser_suite =
  let open Socketio_client.Engineio_client in

  let assert_packets_equal packets1 packets2 =
    assert_equal
      ~printer:(fun packets ->
          String.concat "; "
            (List.map Packet.string_of_t packets))
      packets1
      packets2
  in

  let test_decode_payload_as_binary test_ctxt =
    assert_packets_equal
      [ (Packet.OPEN, Packet.P_None) ]
      (Parser.decode_payload_as_binary "\000\001\2550")
  in

  let test_decode_payload_two_packets test_ctxt =
    assert_packets_equal
      [ (Packet.MESSAGE, Packet.P_String "0")
      ; (Packet.PONG, Packet.P_None)
      ]
      (Parser.decode_payload_as_binary "\000\002\25540\000\001\2553")
  in

  let test_encode_payload test_ctxt =
    assert_string_equal
      "\000\006\2552probe"
      (Parser.encode_payload [(Packet.PING, Packet.P_String "probe")])
  in

  [ "Engineio_client.Parser.decode_payload_as_binary" >:: test_decode_payload_as_binary
  ; "Engineio_client.Parser.decode_payload_two_packets" >:: test_decode_payload_two_packets
  ; "Engineio_client.Parser.encode_payload" >:: test_encode_payload
  ]

let engineio_socket_suite =
  let open Socketio_client.Engineio_client in
  let open Lwt.Infix in

  let module Mock_Transport
      (In : sig
        val packet_stream : Packet.t Lwt_stream.t
        val push_packet : Packet.t option -> unit
      end)
      (Out : sig
        val packet_stream : Packet.t Lwt_stream.t
        val push_packet : Packet.t option -> unit
      end) : Transport = struct
    type mock_type = Polling | WebSocket

    type t =
      { mock_type : mock_type
      ; ready_state : ready_state
      ; packet_stream : Packet.t Lwt_stream.t
      ; push_packet : Packet.t option -> unit
      }

    let create_mock mock_type =
      { mock_type
      ; ready_state = Closed
      ; packet_stream = In.packet_stream
      ; push_packet = In.push_packet
      }

    let create_polling _ = create_mock Polling
    let create_websocket _ = create_mock WebSocket

    let name_of_t t =
      match t.mock_type with
      | Polling -> "polling"
      | WebSocket -> "websocket"

    let ready_state_of_t t = t.ready_state
    let packet_stream_of_t t = t.packet_stream
    let push_packet t = t.push_packet

    let open_ t = Lwt.return { t with ready_state = Opening }
    let write t packets =
      packets
      |> List.iter (fun packet -> Out.push_packet (Some packet))
      |> Lwt.return
    let receive t =
      Lwt_stream.peek In.packet_stream >>= fun _ -> Lwt.return_unit
    let close t =
      Out.push_packet (Some (Packet.CLOSE, Packet.P_None));
      Lwt.return { t with ready_state = Closed }

    let on_open t handshake = { t with ready_state = Open }
    let on_close t = { t with ready_state = Closed }

    module Polling = struct
      type poll_error =
        { code : int
        ; body : string
        }

      exception Polling_exception of poll_error
    end

    module WebSocket = struct
      let name = "websocket"
    end
  end
  in

  let handshake_json =
    `Assoc
      [ ("sid", `String "some-sid")
      ; ("upgrades", `List [])
      ; ("pingInterval", `Int 25000)
      ; ("pingTimeout", `Int 60000)
      ]
  in

  let open_packet =
    ( Packet.OPEN
    , Packet.P_String (Yojson.Basic.to_string handshake_json)
    )
  in

  let test_connect test_ctxt =
    let module In = struct
      let packet_stream, push_packet = Lwt_stream.create ()
    end in

    let module Out = struct
      let packet_stream, push_packet = Lwt_stream.create ()
    end in

    let module Socket = Make_Socket(Mock_Transport(In)(Out)) in

    let packet =
      Lwt_main.run
        (Socket.with_connection Uri.empty
           (fun user_packet_stream user_push_packet ->
              In.push_packet (Some open_packet) |> Lwt.return >>= fun () ->
              Lwt.pick
                [ Lwt_unix.timeout 1.0
                ; Lwt_stream.get user_packet_stream
                ]
           ))
    in

    match packet with
    | None -> assert_failure "End of packet stream?"
    | Some (Packet.OPEN,_) -> ()
    | Some (packet,_) ->
      packet
      |> Packet.string_of_packet_type
      |> Printf.sprintf "Unexpected packet: %s"
      |> assert_failure
  in

  let test_close_sent_on_cancel test_ctxt =
    let module In = struct
      let packet_stream, push_packet = Lwt_stream.create ()
    end in

    let module Out = struct
      let packet_stream, push_packet = Lwt_stream.create ()
    end in

    let module Socket = Make_Socket(Mock_Transport(In)(Out)) in

    let waiter, wakener = Lwt.task () in

    let socket_thread =
      Socket.with_connection Uri.empty
        (fun user_packet_stream user_push_packet ->
           In.push_packet (Some open_packet) |> Lwt.return >>= fun () ->
           Lwt.pick
             [ (Lwt_unix.sleep 2.0 >>= fun () ->
                Lwt_log.notice "User thread finished successfully.")
             ; waiter
             ]
        )
    in

    let () =
      Lwt.async
        (fun () ->
           Lwt_unix.sleep 0.1 >>= fun () ->
           Lwt.wakeup_exn wakener Lwt.Canceled |> Lwt.return
        )
    in

    let sent_packets =
      Lwt_main.run
        (Lwt.catch
           (fun () -> socket_thread)
           (fun exn -> Lwt.return_unit) >>= fun () ->
         Out.push_packet None |> Lwt.return >>= fun () ->
         Lwt_stream.to_list Out.packet_stream)
    in

    let printer ps =
      ps
      |> List.map Packet.string_of_packet_type
      |> String.concat ","
    in

    assert_equal ~printer
      [Packet.PING ; Packet.CLOSE]
      (List.map fst sent_packets)
  in

  [ "Engineio_client.Socket.with_connection" >:: test_connect
  ; "Engineio_client.Socket.with_connection closes when the user thread is canceled"
    >:: test_close_sent_on_cancel
  ]

let socketio_parser_suite =
  let open Socketio_client in

  let string_of_packet packet =
    let brief = Packet.string_of_t packet in
    Printf.sprintf "%s: %s"
      brief
      (match packet with
       | Packet.EVENT (_, args, _, nsp) ->
         Printf.sprintf "%s%s"
           (match nsp with
            | Some nsp -> Printf.sprintf "nsp:%s " nsp
            | None -> "")
           (args
            |> List.map Yojson.Basic.to_string
            |> String.concat ", ")
       | _ -> "_")
  in

  let assert_packet_equal packet1 packet2 =
    assert_equal
      ~printer:string_of_packet
      packet1
      packet2
  in

  let test_decode_connect test_ctxt =
    assert_packet_equal
      (Packet.CONNECT None)
      (Parser.decode_packet "0")
  in

  let test_decode_connect_namespace test_ctxt =
    assert_packet_equal
      (Packet.CONNECT (Some "/namespace"))
      (Parser.decode_packet "0/namespace")
  in

  let test_decode_event test_ctxt =
    assert_packet_equal
      (Packet.EVENT ("my_event", [], None, None))
      (Parser.decode_packet "2[\"my_event\"]")
  in

  let test_decode_event_ack test_ctxt =
    assert_packet_equal
      (Packet.EVENT ("my_event", [], Some 1, None))
      (Parser.decode_packet "21[\"my_event\"]")
  in

  let test_decode_event_args test_ctxt =
    assert_packet_equal
      (Packet.EVENT ("my_event", [`String "arg_one"; `Assoc [("key", `String "val")]], Some 1, None))
      (Parser.decode_packet "21[\"my_event\",\"arg_one\",{\"key\":\"val\"}]")
  in

  let test_decode_event_namespace test_ctxt =
    assert_packet_equal
      (Packet.EVENT ("my_event", [], None, Some "/a-namespace"))
      (Parser.decode_packet "2/a-namespace,[\"my_event\"]")
  in

  let test_decode_event_namespace_ack test_ctxt =
    assert_packet_equal
      (Packet.EVENT ("my_event", [`String "my_arg"], Some 3, Some "/a-namespace"))
      (Parser.decode_packet "2/a-namespace,3[\"my_event\",\"my_arg\"]")
  in

  let test_decode_ack test_ctxt =
    assert_packet_equal
      (Packet.ACK ([`String "arg_one"], 1, None))
      (Parser.decode_packet "31[\"arg_one\"]")
  in

  let test_decode_error test_ctxt =
    assert_packet_equal
      (Packet.ERROR "This is an error.")
      (Parser.decode_packet "4This is an error.")
  in

  let test_encode_event_namespace test_ctxt =
    assert_string_equal
      "2/a-namespace,[\"my_event\"]"
      (Packet.event "my_event" [] ~namespace:"/a-namespace"
       |> Parser.encode_packet)
  in

  let test_encode_event_namespace_ack test_ctxt =
    assert_string_equal
      "2/a-namespace,3[\"my_event\"]"
      (Packet.event "my_event" [] ~ack:3 ~namespace:"/a-namespace"
       |> Parser.encode_packet)
  in

  let test_encode_ack_namespace test_ctxt =
    assert_string_equal
      "3/a-namespace,0[\"my_arg\"]"
      (Packet.ack 0 [`String "my_arg"] ~namespace:"/a-namespace"
       |> Parser.encode_packet)
  in

  [ "Socketio_client.Parser.decode_packet CONNECT" >:: test_decode_connect
  ; "Socketio_client.Parser.decode_packet CONNECT with namespace" >:: test_decode_connect_namespace
  ; "Socketio_client.Parser.decode_packet EVENT" >:: test_decode_event
  ; "Socketio_client.Parser.decode_packet EVENT with ack" >:: test_decode_event_ack
  ; "Socketio_client.Parser.decode_packet EVENT with args" >:: test_decode_event_args
  ; "Socketio_client.Parser.decode_packet EVENT with namespace" >:: test_decode_event_namespace
  ; "Socketio_client.Parser.decode_packet EVENT with namespace and ack" >:: test_decode_event_namespace_ack
  ; "Socketio_client.Parser.decode_packet ACK" >:: test_decode_ack
  ; "Socketio_client.Parser.decode_packet ERROR" >:: test_decode_error
  ; "Socketio_client.Parser.encode_packet EVENT with namespace" >:: test_encode_event_namespace
  ; "Socketio_client.Parser.encode_packet EVENT with namespace and ack" >:: test_encode_event_namespace_ack
  ; "Socketio_client.Parser.encode_packet ACK with namespace" >:: test_encode_ack_namespace
  ]


let () =
  run_test_tt_main
    ("suite" >:::
     List.concat
       [ engineio_parser_suite
       ; engineio_socket_suite
       ; socketio_parser_suite
       ])
