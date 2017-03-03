(** Engine.io client *)

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

module Packet : sig
  (** Packets *)

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
  val string_of_t : t -> string

  val string_of_packet_type : packet_type -> string
  val packet_type_of_int : int -> packet_type
  val int_of_packet_type : packet_type -> int

  val string_of_packet_data : packet_data -> string
end

module Parser : sig
  (** Exposed for testing; not part of the public API. *)

  val decode_payload_as_binary : string -> Packet.t list
  val encode_payload : Packet.t list -> string

  type handshake =
    { sid : string
    ; ping_interval : int
    ; ping_timeout : int
    ; upgrades : string list
    }
end

module Socket : sig
  (** Sockets *)

  (** Connect to an Engine.io server.

      [with_connection uri f] opens a connection to the server at [uri].

      [uri] will be something like ["http://localhost:3000/engine.io"].

      The callback function [f] will be passed a [packet_stream] and a [send]
      function as arguments. The [send] function can be used to send [MESSAGE]
      packets over the socket.

      Protocol-level packets, such as [OPEN], [CLOSE], [PING], [PONG] and [UPGRADE], are handled by
      [with_connection] itself.

      When the callback function [f] terminates, the socket is closed, and the
      result of [f] is returned.
  *)
  val with_connection : Uri.t -> ((Packet.t Lwt_stream.t) -> (string -> unit Lwt.t) -> 'a Lwt.t) -> 'a Lwt.t
end

type ready_state =
  | Opening
  | Open
  | Closed

module type Transport = sig
  type t

  val create_polling : Uri.t -> t
  val create_websocket : Uri.t -> t

  val name_of_t : t -> string
  val ready_state_of_t : t -> ready_state
  val packet_stream_of_t : t -> Packet.t Lwt_stream.t
  val push_packet : t -> Packet.t option -> unit

  val open_ : t -> t Lwt.t
  val write : t -> Packet.t list -> unit Lwt.t
  val receive : t -> unit Lwt.t
  val close : t -> t Lwt.t

  val on_open : t -> Parser.handshake -> t
  val on_close : t -> t

  module Polling : sig
    type poll_error =
      { code : int
      ; body : string
      }

    exception Polling_exception of poll_error
  end

  module WebSocket : sig
    val name : string
  end
end

module Make_Socket(T : Transport) : module type of Socket
