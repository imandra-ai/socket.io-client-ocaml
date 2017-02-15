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

open Lwt.Infix

module Packet = struct
  type t =
    | CONNECT of string option
    | DISCONNECT
    | EVENT of string * Yojson.Basic.json list * int option * string option
    | ACK of Yojson.Basic.json list * int * string option
    | ERROR of string
    | BINARY_EVENT
    | BINARY_ACK

  let int_of_t : t -> int =
    function
    | CONNECT _ -> 0
    | DISCONNECT -> 1
    | EVENT _ -> 2
    | ACK _ -> 3
    | ERROR _ -> 4
    | BINARY_EVENT -> 5
    | BINARY_ACK -> 6

  let string_of_t : t -> string =
    function
    | CONNECT None -> Printf.sprintf "CONNECT[/]"
    | CONNECT (Some namespace) -> Printf.sprintf "CONNECT[%s]" namespace
    | DISCONNECT -> "DISCONNECT"
    | EVENT (name, _, _, _) -> Printf.sprintf "EVENT[%s]" name
    | ACK (_, ack_id, _)-> Printf.sprintf "ACK[%i]" ack_id
    | ERROR msg -> Printf.sprintf "ERROR: %s" msg
    | BINARY_EVENT -> "BINARY_EVENT"
    | BINARY_ACK -> "BINARY_ACK"

  (* Helpers *)

  let event : string -> ?ack:int -> ?namespace:string -> Yojson.Basic.json list -> t =
    fun name ?ack ?namespace arguments ->
      EVENT (name, arguments, ack, namespace)

  let ack : int -> ?namespace:string -> Yojson.Basic.json list -> t =
    fun ack_id ?namespace arguments ->
      ACK (arguments, ack_id, namespace)

  let with_namespace : nsp:(string option) -> t -> t =
    fun ~nsp -> function
      | EVENT (name, args, ack, _) -> EVENT (name, args, ack, nsp)
      | ACK (ack_id, args, _) -> ACK (ack_id, args, nsp)
      | packet -> packet
end

module Parser = struct
  module P = struct
    open Angstrom
    open Eio_util.Angstrom

    let packet_connect : Packet.t Angstrom.t =
      option None
        (many1 any_char >>| fun chars -> Some (Stringext.of_list chars))
      >>| fun namespace -> Packet.CONNECT namespace

    let option_namespace : string option Angstrom.t =
      option None
        (char '/' *>
         (take_while1 (function | ',' -> false | _ -> true)
          >>| fun namespace -> Some (Printf.sprintf "/%s" namespace))
         <* char ',')

    let option_ack : int option Angstrom.t =
      option None (any_integer >>| fun ack_id -> Some ack_id)

    let packet_event : Packet.t Angstrom.t =
      option_namespace >>= fun namespace ->
      option_ack >>= fun ack ->
      json_until_end_of_input >>= function
      | `List (`String event_name :: arguments) ->
        return (Packet.EVENT (event_name, arguments, ack, namespace))
      | _ -> fail "EVENT arguments did not decode to a Json list"

    let packet_ack : Packet.t Angstrom.t =
      option_namespace >>= fun namespace ->
      any_integer >>= fun ack_id ->
      json_until_end_of_input >>= function
      | `List arguments ->
        return (Packet.ACK (arguments, ack_id, namespace))
      | _ -> fail "ACK arguments did not decode to a Json list"

    let packet_error : Packet.t Angstrom.t =
      any_string_until end_of_input >>| fun message ->
      Packet.ERROR message

    let packet : Packet.t Angstrom.t =
      any_digit >>= fun packet_type ->
      match packet_type with
      | 0 -> packet_connect
      | 1 -> return Packet.DISCONNECT
      | 2 -> packet_event
      | 3 -> packet_ack
      | 4 -> packet_error
      | 5 -> return Packet.BINARY_EVENT
      | 6 -> return Packet.BINARY_ACK
      | _ -> fail (Printf.sprintf "unknown packet type %i" packet_type)
  end

  let decode_packet : string -> Packet.t =
    fun data ->
      match Angstrom.parse_only P.packet (`String data) with
      | Ok packet -> packet
      | Error message ->
        Packet.ERROR (Printf.sprintf "Error decoding packet: %s" message)

  let encode_packet : Packet.t -> string =
    fun packet ->
      match packet with
      | Packet.CONNECT namespace ->
        Printf.sprintf "%i%s"
          (Packet.int_of_t packet)
          (Eio_util.Option.value ~default:"" namespace)
      | Packet.DISCONNECT ->
        Printf.sprintf "%i"
          (Packet.int_of_t packet)
      | Packet.EVENT (event_name, data, ack, nsp) ->
        Printf.sprintf "%i%s%s%s"
          (Packet.int_of_t packet)
          (nsp
           |> Eio_util.Option.value_map ~default:""
             ~f:(fun nsp -> Printf.sprintf "%s," nsp))
          (ack
           |> Eio_util.Option.value_map ~default:""
             ~f:string_of_int)
          (Yojson.Basic.to_string (`List (`String event_name :: data)))
      | Packet.ACK (data, ack_id, namespace) ->
        Printf.sprintf "%i%s%i%s"
          (Packet.int_of_t packet)
          (namespace
           |> Eio_util.Option.value_map ~default:""
             ~f:(fun nsp -> Printf.sprintf "%s," nsp))
          ack_id
          (Yojson.Basic.to_string (`List data))
      | _ ->
        raise
          (Invalid_argument
             (Printf.sprintf "Encoding %s: not implemented"
                (Packet.string_of_t packet)))
end

module Socket = struct
  let section =
    Lwt_log.Section.make "socketio.socket"

  type ready_state =
    | Connecting
    | Connected
    | Disconnected

  let string_of_ready_state = function
    | Connecting -> "Connecting"
    | Connected -> "Connected"
    | Disconnected -> "Disconnected"

  type t =
    { uri : Uri.t
    ; ready_state : ready_state
    ; ids : int
    ; namespace : string option
    (* None means the default namespace, "/" *)
    ; acks : int list
    }

  let create : Uri.t -> string option -> t =
    fun uri namespace ->
      { uri = uri
      ; ready_state = Connecting
      ; ids = 0
      ; namespace = namespace
      ; acks = []
      }

  (* Socket.io packet handlers *)

  let on_connect socket namespace =
    Lwt.return
      { socket with
        ready_state =
          if namespace = socket.namespace then
            Connected
          else
            socket.ready_state
      }

  let on_disconnect socket =
    Lwt.return
      { socket with
        ready_state = Disconnected
      }

  let on_packet socket packet =
    Lwt_log.info_f ~section "on_packet %s"
      (Packet.string_of_t packet) >>= fun () ->
    match packet with
    | Packet.CONNECT namespace -> on_connect socket namespace
    | Packet.DISCONNECT -> on_disconnect socket
    | _ -> Lwt.return socket

  (* Engine.io packet handlers *)

  let on_open socket =
    match socket.namespace with
    | None -> Lwt.return (socket, [])
    | Some namespace ->
      Lwt.return
        (socket, [Packet.CONNECT socket.namespace])

  let on_message socket packet_data =
    let data = Engineio_client.Packet.string_of_packet_data packet_data in
    let packet = Parser.decode_packet data  in
    on_packet socket packet
    |> Lwt.map (fun socket -> (socket, packet))

  let on_error socket packet_data =
    let data = Engineio_client.Packet.string_of_packet_data packet_data in
    let packet = Packet.ERROR data in
    Lwt.return (socket, packet)

  let process_eio_packet
    : t * Packet.t list * Packet.t list
      -> Engineio_client.Packet.t
      -> (t * Packet.t list * Packet.t list) Lwt.t =
    fun acc eio_packet ->
      let (socket, acc_packets_received_rev, acc_packets_to_send) = acc in
      let (eio_packet_type, eio_packet_data) = eio_packet in

      Lwt_log.info_f ~section "on_eio_packet %s"
        (Engineio_client.Packet.string_of_packet_type
           eio_packet_type) >>= fun () ->

      match eio_packet_type with
      | Engineio_client.Packet.OPEN ->
        on_open socket
        |> Lwt.map (fun (socket, packets_to_send) ->
            ( socket
            , acc_packets_received_rev
            , List.append acc_packets_to_send packets_to_send
            ))

      | Engineio_client.Packet.MESSAGE ->
        on_message socket eio_packet_data >>= fun (socket, packet_received) ->
        Lwt.return
          ( socket
          , packet_received :: acc_packets_received_rev
          , acc_packets_to_send
          )

      | Engineio_client.Packet.ERROR ->
        on_error socket eio_packet_data >>= fun (socket, packet_received) ->
        Lwt.return
          ( socket
          , packet_received :: acc_packets_received_rev
          , acc_packets_to_send
          )

      | _ ->
        Lwt.return
          ( socket
          , acc_packets_received_rev
          , acc_packets_to_send
          )

  (* Entry point *)

  let with_connection
    : 'a. Uri.t
      -> ?namespace:string
      -> ((Packet.t Lwt_stream.t) -> (Packet.t -> unit Lwt.t) -> 'a Lwt.t)
      -> 'a Lwt.t =
    fun uri ?namespace f ->
      (* packets received *)
      let (packets_recv_stream, push_packet_recv) =
        Lwt_stream.create () in

      (* packets to send *)
      let (packets_send_stream, push_packet_send) =
        Lwt_stream.create () in
      let send packet =
        push_packet_send (Some packet); Lwt.return_unit in

      (* start the user thread *)
      let user_promise =
        f packets_recv_stream send in

      (* create the socket *)
      let socket = create uri namespace in

      let sleep_until_packet_to_send () =
        Lwt_stream.peek packets_send_stream >>= fun _ ->
        Lwt_log.info ~section "Waking to send a packet"
      in

      let sleep_until_packet_received packet_stream =
        Lwt_stream.peek packet_stream >>= fun _ ->
        Lwt_log.info ~section "Waking to process a packet"
      in

      Engineio_client.Socket.with_connection uri
        (fun eio_packet_stream send_eio_message ->

           (* Flush packets from the user to the Engine.io socket, if the
              Socket.io socket is connected.
           *)
           let flush_user_packets socket =
             match socket.ready_state with
             | Connected ->
               packets_send_stream
               |> Lwt_stream.get_available
               |> Lwt_list.iter_s
                 (fun packet ->
                    packet
                    |> Packet.with_namespace ~nsp:socket.namespace
                    |> Parser.encode_packet
                    |> send_eio_message)
             | _ ->
               Lwt_log.debug_f ~section "Socket is %s: not flushing."
                 (string_of_ready_state socket.ready_state)
           in

           let disconnect socket =
             match socket.ready_state with
             | Connected ->
               Packet.DISCONNECT
               |> Parser.encode_packet
               |> send_eio_message >>= fun () ->
               Lwt.return { socket with ready_state = Disconnected }
             | _ ->
               Lwt.return socket
           in

           let maybe_disconnect socket user_promise =
             if Lwt.is_sleeping user_promise then
               Lwt.return socket
             else
               (* User thread has finished; close the socket *)
               Lwt_log.info ~section
                 "User thread has finished; disconnecting." >>= fun () ->
               disconnect socket
           in

           let rec maintain_connection socket user_promise =
             (* Wait for something to do. *)
             let sleep_promises =
               Lwt.pick
                 (List.concat
                    [ (match socket.ready_state with
                          | Connected -> [sleep_until_packet_to_send ()]
                          | _ -> [])
                    ; [ sleep_until_packet_received eio_packet_stream ]
                    ])
             in

             Lwt.choose
               (List.concat
                  [ [ sleep_promises ]
                  ; if Lwt.is_sleeping user_promise then
                      [user_promise >>= fun _ -> Lwt.return_unit]
                    else
                      []
                  ]) >>= fun () ->

             (* Explicitly cancel the sleep promises. *)
             let () = Lwt.cancel sleep_promises in

             (* Process packets received from the Engine.io socket. *)
             eio_packet_stream
             |> Lwt_stream.get_available
             |> Lwt_list.fold_left_s process_eio_packet (socket, [], [])
             >>= fun (socket, packets_received_rev, control_packets_to_send) ->

             (* Push any Socket.io packets received to the user. *)
             let () =
               packets_received_rev
               |> List.rev
               |> List.iter (fun packet -> push_packet_recv (Some packet))
             in

             (* Push any Socket.io control packets to the Engine.io socket.*)
             control_packets_to_send
             |> Lwt_list.iter_s
               (fun packet ->
                  Parser.encode_packet packet |> send_eio_message) >>= fun () ->

             (* Disconnect if the user callback has returned. *)
             maybe_disconnect socket user_promise >>= fun socket ->

             (* Flush user Socket.io packets to the Engine.io socket. *)
             flush_user_packets socket >>= fun () ->

             match socket.ready_state with
             | Disconnected ->
               Lwt_log.debug ~section
                 "Socket is Disconnected, now waiting for user promise to terminate." >>= fun () ->
               user_promise >>= fun x ->
               Lwt_log.debug ~section
                 "User promise has terminated." >>= fun () ->
               Lwt.return x
             | _ ->
               maintain_connection socket user_promise
           in

           maintain_connection socket user_promise
        )
end
