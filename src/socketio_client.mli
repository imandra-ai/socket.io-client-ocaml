(** Socket.io client *)

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

  (** {3 Packet types} *)

  type t =
    | CONNECT of string option
    (** [CONNECT None]: the socket is connected on the default namespace ["/"].

        [CONNECT (Some nsp)]: the socket is connected on the namespace [nsp].
    *)
    | DISCONNECT
    | EVENT of string * Yojson.Basic.json list * int option * string option
    (** [EVENT (event_name, args, ack, namespace)] *)
    | ACK of Yojson.Basic.json list * int * string option
    (** [ACK (args, ack_id, namespace)] *)
    | ERROR of string
    (** [ERROR message] *)
    | BINARY_EVENT
    (** Not implemented. *)
    | BINARY_ACK
    (** Not implemented. *)

  (** The code for this packet's type. *)
  val int_of_t : t -> int

  (** A human-readable string representation. *)
  val string_of_t : t -> string

  (** {3 Helpers for constructing packets} *)

  val event : string -> ?ack:int -> ?namespace:string -> Yojson.Basic.json list -> t
  val ack : int -> ?namespace:string -> Yojson.Basic.json list -> t
end

module Parser : sig
  (** Exposed for testing; not part of the public API. *)

  val decode_packet : string -> Packet.t
  val encode_packet : Packet.t -> string
end

module Socket : sig
  (** Sockets *)

  (** Connect to a Socket.io server.

      [with_connection uri ~namespace f] opens a connection to the server at
      [uri] on namespace [namespace].

      [uri] will be something like ["http://localhost:3000/socket.io"].

      If [namespace] is omitted, the default namespace (["/"]) is used.

      The callback function [f] will be passed a [packet_stream] and a [send]
      function as arguments. The [send] function can be used to send packets
      over the socket.

      When the callback function [f] terminates, the socket is closed, and the
      result of [f] is returned.
  *)
  val with_connection : Uri.t -> ?namespace:string -> ((Packet.t Lwt_stream.t) -> (Packet.t -> unit Lwt.t) -> 'a Lwt.t) -> 'a Lwt.t
end

module Engineio_client : module type of Engineio_client
