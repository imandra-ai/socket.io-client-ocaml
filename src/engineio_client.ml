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

type ready_state =
  | Opening
  | Open
  | Closed

let string_of_ready_state : ready_state -> string =
  function
  | Opening -> "Opening"
  | Open -> "Open"
  | Closed -> "Closed"

module Packet = struct
  type packet_type =
    | OPEN
    | CLOSE
    | PING
    | PONG
    | MESSAGE
    | UPGRADE
    | NOOP
    | ERROR

  type packet_data =
    | P_None
    | P_String of string
    | P_Binary of Lwt_bytes.t

  type t = packet_type * packet_data

  let string_of_packet_type : packet_type -> string =
    function
    | OPEN -> "OPEN"
    | CLOSE -> "CLOSE"
    | PING -> "PING"
    | PONG -> "PONG"
    | MESSAGE -> "MESSAGE"
    | UPGRADE -> "UPGRADE"
    | NOOP -> "NOOP"
    | ERROR -> "ERROR"

  let packet_type_of_int : int -> packet_type =
    function
    | 0 -> OPEN
    | 1 -> CLOSE
    | 2 -> PING
    | 3 -> PONG
    | 4 -> MESSAGE
    | 5 -> UPGRADE
    | 6 -> NOOP
    | _ -> ERROR

  let int_of_packet_type : packet_type -> int =
    function
    | OPEN -> 0
    | CLOSE -> 1
    | PING -> 2
    | PONG -> 3
    | MESSAGE -> 4
    | UPGRADE -> 5
    | NOOP -> 6
    | ERROR -> -1

  let string_of_packet_data : packet_data -> string =
    function
    | P_None -> ""
    | P_String data -> data
    | P_Binary bytes -> Lwt_bytes.to_string bytes

  let is_binary : packet_data -> bool =
    function
    | P_None -> false
    | P_String data -> false
    | P_Binary data -> true

  let string_of_t : t -> string =
    fun (packet_type, packet_data) ->
      Printf.sprintf "%s: %s"
        (string_of_packet_type packet_type)
        (string_of_packet_data packet_data)

  let close : t =
    (CLOSE, P_None)

  let ping : t =
    (PING, P_None)

  let ping_probe : t =
    (PING, P_String "probe")

  let upgrade : t =
    (UPGRADE, P_None)

  let message : string -> t =
    fun string ->
      (MESSAGE, P_String string)
end

module Parser = struct

  let protocol : int = 3

  (* See https://github.com/socketio/engine.io-protocol#encoding *)

  type frame_encoding = [`String | `Binary]

  module P = struct
    open Angstrom
    open Eio_util.Angstrom

    let packet : frame_encoding -> Packet.t Angstrom.t =
      fun frame_encoding ->
        let packet_data_of_string data =
          match frame_encoding with
          | `String -> Packet.P_String data
          | `Binary -> Packet.P_Binary (Lwt_bytes.of_string data)
        in
        let p_data =
          (end_of_input >>| fun () -> Packet.P_None)
          <|>
          (any_string_until end_of_input >>| packet_data_of_string)
        in
        any_digit >>| Packet.packet_type_of_int >>= fun packet_type ->
        p_data >>| fun packet_data ->
        (packet_type, packet_data)

    let frame_encoding_flag : frame_encoding Angstrom.t =
      choice
        [ (char '\000' >>| fun _ -> `String)
        ; (char '\001' >>| fun _ -> `Binary)
        ]

    let frame_length : int Angstrom.t =
      many (not_char '\255') <* char '\255' >>| fun digits ->
      digits
      |> List.map Char.code
      |> List.map string_of_int
      |> String.concat ""
      |> int_of_string

    let frame : (frame_encoding * string) Angstrom.t =
      frame_encoding_flag >>= fun frame_encoding ->
      frame_length >>= fun len ->
      take len >>| fun frame_data ->
      (frame_encoding, frame_data)

    let payload : (frame_encoding * string) list Angstrom.t =
      many frame <* end_of_input
  end

  let decode_packet : frame_encoding -> string -> Packet.t =
    fun frame_encoding data ->
      match Angstrom.parse_only (P.packet frame_encoding) (`String data) with
      | Ok packet ->
        packet
      | Error message ->
        (Packet.ERROR, Packet.P_String (Printf.sprintf "Packet parse error: %s" message))

  let decode_packet_string : string -> Packet.t =
    fun data ->
      decode_packet `String data

  let decode_packet_binary : string -> Packet.t =
    fun data ->
      decode_packet `Binary data

  let decode_payload_as_binary : string -> Packet.t list =
    fun data ->
      match Angstrom.parse_only P.payload (`String data) with
      | Error message ->
        [(Packet.ERROR, Packet.P_String (Printf.sprintf "Payload parse error: %s" message))]
      | Ok frames ->
        frames
        |> List.map
          (fun (frame_encoding, frame_data) ->
             decode_packet frame_encoding frame_data)

  let encode_packet : Packet.t -> string =
    fun (packet_type, packet_data) ->
      Printf.sprintf "%i%s"
        (Packet.int_of_packet_type packet_type)
        (Packet.string_of_packet_data packet_data)

  let encode_payload : Packet.t list -> string =
    fun packets ->
      let encode_one_packet (packet_type, packet_data) =
        let bin_flag =
          if Packet.is_binary packet_data then
            '\001'
          else
            '\000'
        in
        let data_as_string = Packet.string_of_packet_data packet_data in
        let payload_length =
          (* the length of the data plus one for the packet type *)
          1 + (String.length data_as_string)
        in
        let length_as_char_digits =
          (* convert the integer length of the packet_data to a byte string *)
          payload_length                (* 97 *)
          |> string_of_int              (* -> "97" *)
          |> Stringext.to_list          (* -> ['9'; '7']*)
          |> List.map Stringext.of_char (* -> ["9"; "7"] *)
          |> List.map int_of_string     (* -> [9; 7] *)
          |> List.map Char.chr          (* -> ['\009'; '\007'] *)
          |> Stringext.of_list          (* -> "\009\007" *)
        in
        Printf.sprintf "%c%s%c%i%s"
          bin_flag
          length_as_char_digits
          (Char.chr 255)
          (Packet.int_of_packet_type packet_type)
          data_as_string
      in
      packets
      |> List.map encode_one_packet
      |> String.concat ""

  type handshake =
    { sid : string
    ; ping_interval : int
    ; ping_timeout : int
    ; upgrades : string list
    }

  let string_of_handshake : handshake -> string =
    fun handshake ->
      Format.sprintf
        "sid: '%s' ping_interval: %i ping_timeout: %i"
        handshake.sid
        handshake.ping_interval
        handshake.ping_timeout

  let parse_handshake : Packet.packet_data -> handshake =
    fun packet_data ->
      match packet_data with
      | Packet.P_None -> raise (Invalid_argument "no data")
      | Packet.P_Binary _ -> raise (Invalid_argument "binary")
      | Packet.P_String string ->
        Yojson.Basic.(
          match from_string string with
          | `Assoc assoc ->
            let sid =
              match List.assoc "sid" assoc with
              | `String sid -> sid
              | _ -> raise (Invalid_argument "expected sid to be a string")
            in
            let upgrades =
              match List.assoc "upgrades" assoc with
              | `List ls ->
                ls
                |> List.map (function
                    | `String upgrade -> upgrade
                    | _ -> raise (Invalid_argument "expected upgrade to be a string")
                  )
              | _ -> raise (Invalid_argument "expected upgrades to be a list")
            in
            let ping_interval =
              match List.assoc "pingInterval" assoc with
              | `Int interval -> interval
              | _ -> raise (Invalid_argument "expected pingInterval to be an int")
            in
            let ping_timeout =
              match List.assoc "pingTimeout" assoc with
              | `Int timeout -> timeout
              | _ -> raise (Invalid_argument "expected pingTimeout to be an int")
            in
            { sid = sid
            ; ping_interval = ping_interval
            ; ping_timeout = ping_timeout
            ; upgrades = upgrades
            }
          | _ -> raise (Invalid_argument "expected an object")
        )
end

module Transport = struct
  let log_receive_packets : section:Lwt_log.section -> Packet.t list -> unit Lwt.t =
    fun ~section packets ->
      Lwt_log.info_f ~section "receive packets [%s]"
        (packets
         |> List.map fst
         |> List.map Packet.string_of_packet_type
         |> String.concat ", ")

  let log_write_packets : section:Lwt_log.section -> Packet.t list -> unit Lwt.t =
    fun ~section packets ->
      Lwt_log.info_f ~section "write packets [%s]"
        (packets
         |> List.map fst
         |> List.map Packet.string_of_packet_type
         |> String.concat ", ")

  module Polling = struct
    let section : Lwt_log.section =
      Lwt_log.Section.make "eio.transport.polling"

    type t =
      { ready_state : ready_state
      ; uri : Uri.t
      ; packets : Packet.t Lwt_stream.t
      ; push_packet : Packet.t option -> unit
      }

    let name : string = "polling"

    let create : Uri.t -> t =
      fun uri ->
        let packets, push_packet = Lwt_stream.create () in
        { ready_state = Closed
        ; uri =
            Uri.add_query_param uri ("transport", [name])
        ; packets =
            packets
        ; push_packet =
            push_packet
        }

    let log_packet : Packet.t -> unit Lwt.t =
      fun (packet_type, packet_data) ->
        Lwt_log.debug_f ~section "Decoded packet %s with data: '%s'"
          (Packet.string_of_packet_type packet_type)
          (match packet_data with
           | Packet.P_None -> "no data"
           | Packet.P_String string -> string
           | Packet.P_Binary bytes -> Format.sprintf "binary packet_data of length %i" (Lwt_bytes.length bytes))

    type poll_error =
      { code : int
      ; body : string
      }

    exception Polling_exception of poll_error

    let process_response : t -> Cohttp_lwt_unix.Response.t * Cohttp_lwt_body.t -> unit Lwt.t =
      fun t (resp, body) ->
        Cohttp.(Cohttp_lwt_unix.(
            let code =
              resp
              |> Response.status
              |> Code.code_of_status in
            Lwt_log.debug_f ~section "Received status code: %i" code >>= fun () ->
            if Code.is_success code then
              Cohttp_lwt_body.to_string_list body
              >>= Lwt_list.map_s
                (fun line ->
                   Lwt_log.debug_f ~section "Got line in body: '%s'" (String.escaped line) >>= fun () ->
                   let packets = Parser.decode_payload_as_binary line in
                   Lwt_list.iter_s log_packet packets >>= fun () ->
                   Lwt.return packets)
              >>= fun packets_list ->
              let packets = List.concat packets_list in
              log_receive_packets ~section packets >>= fun () ->
              List.iter (fun packet -> t.push_packet (Some packet)) packets;
              Lwt.return_unit
            else
              Cohttp_lwt_body.to_string body >>= fun body ->
              Lwt_log.error_f ~section "%s" body >>= fun () ->
              Lwt.fail (Polling_exception { code; body })
          ))

    let receive : t -> unit Lwt.t =
      fun t ->
        Lwt.(
          Cohttp.(Cohttp_lwt_unix.(
              Lwt_log.debug_f ~section "GET '%s'" (Uri.to_string t.uri) >>= fun () ->
              catch
                (fun () ->
                   Client.get
                     ~headers:(Header.init_with "accept" "application/json")
                     t.uri >>= process_response t)
                (fun exn ->
                   Lwt_log.error_f ~section "Poll failed: '%s'" (Printexc.to_string exn) >>= fun () ->
                   match exn with
                   | Failure msg ->
                     Lwt_log.error_f ~section "Poll failed: '%s'" msg
                   | Polling_exception poll_error ->
                     Lwt_log.error_f ~section "Poll failed: %i '%s'" poll_error.code poll_error.body >>= fun () ->
                     fail exn
                   | exn -> fail exn)
            ))
        )

    let open_ : t -> t Lwt.t =
      fun t ->
        match t.ready_state with
        | Closed ->
          receive t >>= fun () ->
          Lwt.return
            { t with ready_state = Opening }
        | _ ->
          Lwt_log.warning_f ~section "Attempted open on %s transport"
            (string_of_ready_state t.ready_state) >>= fun () ->
          Lwt.return t

    let write : t -> Packet.t list -> unit Lwt.t =
      fun t packets ->
        Lwt.(Cohttp.(Cohttp_lwt_unix.(
            let encoded_payload =
              Parser.encode_payload packets
            in
            log_write_packets ~section packets >>= fun () ->
            Lwt_log.debug_f ~section "POST '%s' with data '%s'"
              (Uri.to_string t.uri)
              (encoded_payload |> String.escaped)
            >>= fun () ->
            catch
              (fun () ->
                 Client.post
                   ~headers:(Header.init_with "content-type" "application/octet-stream")
                   ~body:(encoded_payload |> Cohttp_lwt_body.of_string)
                   t.uri >>= fun (resp, body) ->
                 return_unit
              )
              (function
                | Failure msg ->
                  Lwt_log.error_f ~section "Write failed: '%s'" msg >>= fun () ->
                  return_unit
                | exn -> fail exn)
          )))

    let on_open : t -> Parser.handshake -> t =
      fun t handshake ->
        { t with
          ready_state = Open
        ; uri =
            t.uri
            |> (Eio_util.flip Uri.remove_query_param) "sid"
            |> (Eio_util.flip Uri.add_query_param) ("sid", [Parser.(handshake.sid)])
        }

    let on_close : t -> t =
      fun t ->
        { t with
          ready_state = Closed
        ; uri =
            t.uri
            |> (Eio_util.flip Uri.remove_query_param) "sid"
        }

    let close : t -> t Lwt.t =
      fun t ->
        write t [Packet.close] >>= fun () ->
        Lwt.return (on_close t)
  end

  module WebSocket = struct
    open Websocket_lwt

    let section : Lwt_log_core.section =
      Lwt_log.Section.make "eio.transport.websocket"

    type t =
      { ready_state : ready_state
      ; uri : Uri.t
      ; connection : ((unit -> Frame.t Lwt.t) * (Frame.t -> unit Lwt.t)) option
      ; packets : Packet.t Lwt_stream.t
      ; push_packet : Packet.t option -> unit
      }

    let name : string = "websocket"

    let create : Uri.t -> t =
      fun uri ->
        let packets, push_packet = Lwt_stream.create () in
        { ready_state = Closed
        ; uri = Uri.add_query_param uri ("transport", [name])
        ; connection = None
        ; packets = packets
        ; push_packet = push_packet
        }

    let open_ : t -> t Lwt.t =
      fun t ->
        match t.ready_state with
        | Closed ->
          Resolver_lwt.resolve_uri ~uri:t.uri Resolver_lwt_unix.system >>= fun endp ->
          Conduit_lwt_unix.(
            endp_to_client ~ctx:default_ctx endp >>= fun client ->
            Websocket_lwt.with_connection ~ctx:default_ctx client t.uri
          ) >>= fun (recv, send) ->
          Lwt.return
            { t with
              ready_state = Open
            ; connection = Some (recv, send)
            }
        | _ ->
          Lwt_log.warning_f ~section "Attempted open on %s transport"
            (string_of_ready_state t.ready_state) >>= fun () ->
          Lwt.return t

    (* Socket should already be open - we only get a websocket transport via an
       upgrade. *)
    let on_open : t -> Parser.handshake -> t =
      fun t handshake -> t

    let write_frame : t -> Websocket_lwt.Frame.t -> unit Lwt.t =
      fun t frame ->
        match t.connection with
        | None ->
          Lwt_log.info ~section "Write attempt with no connection."
        | Some (recv, send) ->
          Lwt_log.debug_f ~section "Sending frame %s"
            (Frame.show frame |> Stringext.replace_all ~pattern:"\n  " ~with_:" ") >>= fun () ->
          send frame

    let write : t -> Packet.t list -> unit Lwt.t =
      fun t packets ->
        match t.connection with
        | None ->
          Lwt_log.info ~section "Write attempt with no connection."
        | Some (recv, send) ->
          log_write_packets ~section packets >>= fun () ->
          packets
          |> Lwt_list.iter_s
            (fun packet ->
               let frame = (Frame.create ~content:(Parser.encode_packet packet) ()) in
               write_frame t frame)

    let receive : t -> unit Lwt.t =
      fun t ->
        match (t.ready_state, t.connection) with
        | Closed, _
        | _, None ->
          Lwt_log.info ~section "Receive attempt with no connection." >>= fun () ->
          Lwt.return_unit
        | _, Some (recv, send) ->
          let react frame =
            Frame.(
              Lwt_log.debug_f ~section "Received frame %s"
                (Frame.show frame |> Stringext.replace_all ~pattern:"\n  " ~with_:" ") >>= fun () ->
              match frame.opcode with
              | Opcode.Text ->
                let packet = Parser.decode_packet_string frame.content in
                Lwt.return_some packet
              | Opcode.Binary ->
                let packet = Parser.decode_packet_binary frame.content in
                Lwt.return_some packet
              | Opcode.Ping ->
                send (Frame.create ~opcode:Opcode.Pong ()) >>= fun () ->
                Lwt.return_none
              | Opcode.Pong ->
                Lwt.return_none
              | Opcode.Close ->
                (* Translate Websocket Close frame into an Engine.io Close packet. *)
                Lwt.return_some
                  ( Packet.CLOSE
                  , Packet.P_Binary
                      (frame.content |> Lwt_bytes.of_string)
                  )
              | _ ->
                Lwt_log.error_f ~section "Unexpected frame %s"
                  (Frame.show frame) >>= fun () ->
                Lwt.return_none
            )
          in
          Lwt.catch
            (fun () -> recv () >>= react >>= fun packet_opt ->
              log_receive_packets ~section (Eio_util.Option.to_list packet_opt) >>= fun () ->
              let () = Eio_util.Option.iter ~f:(fun packet -> t.push_packet (Some packet)) packet_opt in
              Lwt.return_unit
            )
            (fun exn ->
               Lwt_log.error_f ~section "receive: %s" (Printexc.to_string exn) >>= fun () ->
               Lwt.fail exn)

    let on_close : t -> t =
      fun t ->
        { t with
          ready_state = Closed
        ; connection = None
        ; uri =
            t.uri
            |> (Eio_util.flip Uri.remove_query_param) "sid"
        }

    let close : t -> t Lwt.t =
      fun t ->
        write_frame t (Frame.close 1000) >>= fun () ->
        Lwt.return (on_close t)
  end

  type t =
    | Polling of Polling.t
    | WebSocket of WebSocket.t

  let string_of_t : t -> string =
    function
    | Polling _ -> Polling.name
    | WebSocket _ -> WebSocket.name

  let create_polling : Uri.t -> t =
    fun uri ->
      Polling (Polling.create uri)

  let create_websocket : Uri.t -> t =
    fun uri ->
      WebSocket (WebSocket.create uri)

  let open_ : t -> t Lwt.t =
    function
    | Polling polling ->
      Polling.open_ polling >>= fun polling ->
      Lwt.return (Polling polling)
    | WebSocket websocket ->
      WebSocket.open_ websocket >>= fun websocket ->
      Lwt.return (WebSocket websocket)

  let write : t -> Packet.t list -> unit Lwt.t =
    fun t packets ->
      match t with
      | Polling polling ->
        Polling.write polling packets
      | WebSocket websocket ->
        WebSocket.write websocket packets

  let on_open : t -> Parser.handshake -> t =
    fun t handshake ->
      match t with
      | Polling polling ->
        Polling (Polling.on_open polling handshake)
      | WebSocket websocket ->
        WebSocket (WebSocket.on_open websocket handshake)

  let on_close : t -> t =
    function
    | Polling polling ->
      Polling (Polling.on_close polling)
    | WebSocket websocket ->
      WebSocket (WebSocket.on_close websocket)

  let packet_stream : t -> Packet.t Lwt_stream.t =
    function
    | Polling polling ->
      Polling.(polling.packets)
    | WebSocket websocket ->
      WebSocket.(websocket.packets)

  let push_packet : t -> Packet.t option -> unit =
    fun t packet ->
      match t with
      | Polling polling ->
        Polling.(polling.push_packet packet)
      | WebSocket websocket ->
        WebSocket.(websocket.push_packet packet)

  let receive : t -> unit Lwt.t =
    function
    | Polling polling ->
      Polling.receive polling
    | WebSocket websocket ->
      WebSocket.receive websocket

  let close : t -> t Lwt.t =
    function
    | Polling polling ->
      Polling.close polling >>= fun polling ->
      Lwt.return (Polling polling)
    | WebSocket websocket ->
      WebSocket.close websocket >>= fun websocket ->
      Lwt.return (WebSocket websocket)
end

module Socket = struct
  let section = Lwt_log.Section.make "eio.socket"

  type t =
    { ready_state : ready_state
    ; transport : Transport.t
    ; handshake : Parser.handshake option
    ; ping_sent_at : float option
    ; pong_received_at : float option
    ; uri : Uri.t
    ; probe_promise : Transport.t option Lwt.t option
    ; packets : Packet.t Lwt_stream.t
    ; push_packet : Packet.t option -> unit
    }

  let create : Uri.t -> t =
    fun uri ->
      let packets, push_packet = Lwt_stream.create () in
      let uri =
        Uri.with_query uri [("EIO", [string_of_int Parser.protocol])]
      in
      { ready_state = Closed
      ; transport =
          Transport.create_polling uri
      ; handshake = None
      ; ping_sent_at = None
      ; pong_received_at = None
      ; uri = uri
      ; probe_promise = None
      ; packets = packets
      ; push_packet = push_packet
      }

  let open_ : t -> t Lwt.t =
    fun socket ->
      match socket.ready_state with
      | Closed ->
        Lwt.catch
          (fun () ->
             Transport.open_ socket.transport >>= fun transport ->
             Lwt.return
               { socket with
                 ready_state = Opening
               ; transport = transport
               })
          (fun exn ->
             Lwt_log.error_f ~section "Open failed: %s" (Printexc.to_string exn) >>= fun () ->
             match exn with
             | Transport.Polling.Polling_exception poll_error ->
               let is_transport_unknown =
                 (* TODO: decode the poll_error as Json and use the error code.
                    ("Transport unknown" is code 0.)
                 *)
                 try
                   let _ =
                     Str.search_forward (Str.regexp_string "Transport unknown") Transport.Polling.(poll_error.body) 0
                   in true
                 with
                 | Not_found -> false
               in
               if is_transport_unknown then
                 (* Try again with the websocket transport *)
                 Lwt_log.info ~section
                   "Polling transport was rejected, now opening with websocket transport." >>= fun () ->
                 let socket =
                   { socket with
                     transport =
                       Transport.create_websocket socket.uri
                   }
                 in
                 Transport.open_ socket.transport >>= fun transport ->
                 Lwt.return
                   { socket with
                     ready_state = Opening
                   ; transport = transport
                   }
               else
                 Lwt.fail exn
             | _ -> Lwt.fail exn
          )
      | _ ->
        Lwt_log.warning_f ~section "Attempted open on %s socket"
          (string_of_ready_state socket.ready_state) >>= fun () ->
        Lwt.return socket

  let write : t -> Packet.t list -> unit Lwt.t =
    fun socket packets ->
      match socket.ready_state, packets with
      | Closed, _
      | _, [] -> Lwt.return_unit
      | _, _ -> Transport.write socket.transport packets

  let probe : Uri.t -> Transport.t -> Parser.handshake -> Transport.t option Lwt.t =
    fun uri current_transport handshake ->
      let should_probe =
        List.exists ((=) Transport.WebSocket.name) Parser.(handshake.upgrades) &&
        Transport.string_of_t current_transport <> Transport.WebSocket.name
      in
      if should_probe then
        let open Transport.WebSocket in
        create uri
        |> open_ >>= fun websocket ->
        Lwt_log.info ~section "Probing websocket transport..." >>= fun () ->
        write websocket [Packet.ping_probe] >>= fun () ->
        receive websocket >>= fun () ->
        (Lwt_stream.get websocket.packets >>= function
          | Some (Packet.PONG, Packet.P_String "probe") ->
            Lwt_log.info ~section "Ok to upgrade." >>= fun () ->
            Lwt.return (Some (Transport.WebSocket websocket))
          | Some (packet_type, packet_data) ->
            Lwt_log.error_f ~section "Can not upgrade. Expecting PONG, but got '%s'."
              (Packet.string_of_packet_type packet_type) >>= fun () ->
            Lwt.return_none
          | None ->
            Lwt_log.error ~section "Can not upgrade. Expecting PONG, but didn't get a Packet." >>= fun () ->
            Lwt.return_none)
      else
        Lwt.return_none

  let on_open : t -> Packet.packet_data -> t Lwt.t =
    fun socket packet_data ->
      let handshake = Parser.parse_handshake packet_data in
      Lwt_log.debug_f ~section "Got sid '%s'" Parser.(handshake.sid) >>= fun () ->
      let transport =
        Transport.on_open socket.transport handshake in
      let uri =
        Uri.add_query_param' socket.uri ("sid", Parser.(handshake.sid)) in
      Lwt.return
        { socket with
          ready_state = Open
        ; handshake = Some handshake
        ; transport = transport
        ; uri = uri
        ; probe_promise = Some (probe uri transport handshake)
        }

  let on_pong : t -> t Lwt.t =
    fun socket ->
      let now = Unix.time () in
      Lwt_log.debug_f ~section "PONG received at %.2f" now >>= fun () ->
      Lwt.return
        { socket with
          pong_received_at = Some now
        }

  let on_close : t -> t Lwt.t =
    fun socket ->
      let transport = Transport.on_close socket.transport in
      Lwt.return
        { socket with
          ready_state = Closed
        ; handshake = None
        ; transport = transport
        ; uri =
            Uri.remove_query_param socket.uri "sid"
        }

  let process_packet : t -> Packet.t -> t Lwt.t =
    fun socket (packet_type, packet_data) ->
      Lwt_log.debug_f ~section "process_packet %s"
        (packet_type |> Packet.string_of_packet_type) >>= fun () ->
      let () = socket.push_packet (Some (packet_type, packet_data)) in
      match packet_type with
      | Packet.OPEN -> on_open socket packet_data
      | Packet.PONG -> on_pong socket
      | Packet.CLOSE -> on_close socket
      | _ -> Lwt.return socket

  let close : t -> t Lwt.t =
    fun socket ->
      match socket.ready_state with
      | Closed ->
        Lwt.return socket
      | _ ->
        Transport.close socket.transport >>= fun transport ->
        on_close { socket with transport = transport }

  let log_socket_state : t -> unit Lwt.t =
    fun socket ->
      Lwt_log.debug_f ~section "Socket is %s %s"
        (string_of_ready_state socket.ready_state)
        (Eio_util.Option.value_map socket.handshake
           ~default:"(no handshake)"
           ~f:(fun handshake -> Format.sprintf "(%s)" (Parser.string_of_handshake handshake))) >>= fun () ->
      Lwt_log.debug_f ~section "Transport is %s (%s)"
        (string_of_ready_state
           (match socket.transport with
            | Transport.Polling polling ->
              Transport.Polling.(polling.ready_state)
            | Transport.WebSocket websocket ->
              Transport.WebSocket.(websocket.ready_state)))
        (Transport.string_of_t socket.transport)

  let maybe_poll_again poll_promise socket =
    match socket.ready_state, Lwt.is_sleeping poll_promise with
    | Closed, _ (* socket closed, don't renew *)
    | _, true -> poll_promise (* still polling, don't renew *)
    | _ -> Transport.receive socket.transport (* poll again *)

  let sleep_until_ping socket handshake =
    match socket.ping_sent_at, socket.pong_received_at with
    | None, _ ->
      Lwt_log.info ~section "No ping sent: send ping now"
    | Some ping_sent_at, None ->
      (* We are waiting for PONG from server. Raise Timeout if we
         don't get it in time. *)
      let seconds_since_last_ping =
        Unix.time () -. ping_sent_at in
      let ping_timeout_seconds =
        (float_of_int Parser.(handshake.ping_timeout)) /. 1000.0 in
      let timeout_seconds =
        ping_timeout_seconds -. seconds_since_last_ping in
      Lwt_log.info_f ~section "Waiting %.2f seconds for PONG" timeout_seconds >>= fun () ->
      Lwt_unix.timeout timeout_seconds
    | _, Some pong_received_at ->
      (* All good, send a PING at the next interval. *)
      let seconds_since_last_pong =
        Unix.time () -. pong_received_at in
      let ping_interval_seconds =
        (float_of_int Parser.(handshake.ping_interval)) /. 1000.0 in
      let sleep_seconds =
        ping_interval_seconds -. seconds_since_last_pong in
      Lwt_log.info_f ~section "Will ping in %.2f seconds" sleep_seconds >>= fun () ->
      Lwt_unix.sleep sleep_seconds >>= fun () ->
      Lwt_log.info ~section "Waking to send ping"

  let maybe_send_ping socket queue_packet =
    let should_ping =
      match socket.ready_state, socket.handshake with
      | Closed, _ -> false
      | _, None -> false (* Not connected. *)
      | _, Some handshake ->
        (match socket.ping_sent_at, socket.pong_received_at with
         | None, _ -> true (* No ping sent yet. *)
         | Some _, None -> false (* Ping sent, waiting for pong. *)
         | _, Some pong_received_at ->
           let seconds_since_last_pong = Unix.time () -. pong_received_at in
           let ping_interval_seconds = (float_of_int Parser.(handshake.ping_interval)) /. 1000.0 in
           seconds_since_last_pong >= ping_interval_seconds)
    in
    if should_ping then
      queue_packet Packet.ping >>= fun () ->
      Lwt.return
        { socket with
          ping_sent_at = Some (Unix.time ())
        ; pong_received_at = None
        }
    else
      Lwt.return socket

  let sleep_until_packet_received socket =
    Lwt_stream.peek (Transport.packet_stream socket.transport) >>= fun _ ->
    Lwt_log.info ~section "Waking to process a packet"

  let sleep_until_packet_to_send packets_send_stream =
    Lwt_stream.peek packets_send_stream >>= fun _ ->
    Lwt_log.info ~section "Waking to send a packet"

  let maybe_close socket user_promise =
    if Lwt.is_sleeping user_promise then
      Lwt.return socket
    else
      (* User thread has finished; close the socket *)
      Lwt_log.info ~section "User thread has finished; closing the socket." >>= fun () ->
      close socket

  let forward_until_close old_transport socket =
    let old_packet_stream = Transport.packet_stream old_transport in
    let rec react_forever () =
      Lwt_stream.get old_packet_stream >>= function
      | None
      | Some (Packet.CLOSE, _) ->
        Lwt_log.info ~section "Finished draining packets."
      | Some packet ->
        Lwt_log.info_f ~section "Draining packet %s"
          (Packet.string_of_packet_type (fst packet)) >>= fun () ->
        let () = Transport.push_packet socket.transport (Some packet) in
        react_forever ()
    in
    react_forever ()

  let maybe_switch_transports socket poll_promise =
    match socket.probe_promise with
    | Some promise when not (Lwt.is_sleeping promise) ->
      promise >>= fun transport_opt ->
      let socket =
        { socket with
          probe_promise = None
        }
      in
      (match transport_opt with
       | Some transport ->
         let old_transport = socket.transport in
         let socket =
           { socket with
             transport = transport
           }
         in

         Lwt_log.info ~section "Canceling poll on old transport." >>= fun () ->
         let () = Lwt.cancel poll_promise in
         Eio_util.Lwt.ignore_exn (fun () -> poll_promise) >>= fun () ->

         Lwt_log.info ~section "Sending UPGRADE." >>= fun () ->
         write socket [Packet.upgrade] >>= fun () ->

         Lwt_log.info ~section "Draining old transport." >>= fun () ->
         (* Terminate the old transport's packet stream *)
         let () = Transport.push_packet old_transport None in
         forward_until_close old_transport socket >>= fun () ->

         Lwt.return socket
       | None -> Lwt.return socket)
    | _ -> Lwt.return socket

  let with_connection : 'a. Uri.t -> ((Packet.t Lwt_stream.t) -> (string -> unit Lwt.t) -> 'a Lwt.t) -> 'a Lwt.t =
    fun uri f ->
      (* packets to send via transport *)
      let (packets_send_stream, push_packet_send) =
        Lwt_stream.create () in
      let send packet =
        push_packet_send (Some packet); Lwt.return_unit in

      let rec maintain_connection : 'a. unit Lwt.t -> 'a Lwt.t -> t -> 'a Lwt.t =
        fun poll_promise user_promise socket ->
          log_socket_state socket >>= fun () ->

          (* Create a promise that sleeps until:
             - we need to send a PING;
             - the server has failed to respond with a PONG;
             - the transport recevied a packet; or
             - the user has given us a packet to send.
             When one promise returns, the others are canceled.
          *)
          let sleep_promise =
            Lwt.pick
              (List.concat
                 [ (* If we're connected, wake up for pings. *)
                   (match socket.ready_state, socket.handshake with
                    | Closed, _
                    | _, None -> []
                    | _, Some handshake -> [sleep_until_ping socket handshake])
                 ; [ sleep_until_packet_received socket
                   ; sleep_until_packet_to_send packets_send_stream
                   ]
                 ])
          in

          (* Create a promise that sleeps until one of the following returns:
             - the user's callback (if it hasn't already);
             - the transport upgrade probe (if there is one);
             - the transport polling promise; or
             - one of the sleep promises above.
             When one of these promises returns, the others are NOT canceled.
          *)
          Lwt.choose
            (List.concat
               [ if Lwt.is_sleeping user_promise then [user_promise >>= fun _ -> Lwt.return_unit] else []
               ; socket.probe_promise
                 |> Eio_util.Option.map ~f:(fun p -> p >>= fun _ -> Lwt.return_unit)
                 |> Eio_util.Option.to_list
               ; [ poll_promise
                 ; sleep_promise
                 ]
               ]) >>= fun () ->

          (* Explicitly cancel the sleep promises. *)
          let () = Lwt.cancel sleep_promise in

          (* Process packets receveived over the transport. *)
          Transport.packet_stream socket.transport
          |> Lwt_stream.get_available
          |> Lwt_list.fold_left_s process_packet socket >>= fun socket ->

          (* Send a ping packet if we need to. *)
          maybe_send_ping socket send >>= fun socket ->

          (* Flush the queued packets to the transport. *)
          write socket (Lwt_stream.get_available packets_send_stream) >>= fun () ->

          (* Close the socket if the user callback has returned. *)
          maybe_close socket user_promise >>= fun socket ->

          (* If the upgrade probe was successful, complete the transport
             upgrade. *)
          maybe_switch_transports socket poll_promise >>= fun socket ->

          (* Re-start the promise that waits on new packets from the
             transport. *)
          let poll_promise = maybe_poll_again poll_promise socket in

          (* If the socket is closed, return the user's callback. Otherwise, loop. *)
          match socket.ready_state with
          | Closed ->
            Lwt_log.debug ~section
              "Socket is Closed, now waiting for user promise to terminate."
            >>= fun () ->
            user_promise
          | _ -> maintain_connection poll_promise user_promise socket
      in

      let socket =
        create uri in

      open_ socket >>= fun socket ->

      let poll_promise = Lwt.return_unit in

      let user_promise =
        f socket.packets
          (fun data -> send (Packet.message data))
      in

      maintain_connection poll_promise user_promise socket
end
