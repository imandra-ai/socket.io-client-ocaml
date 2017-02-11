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
  type t =
    | CONNECT of string option
    | DISCONNECT
    | EVENT of string * Yojson.Basic.json list * int option * string option
    | ACK of Yojson.Basic.json list * int * string option
    | ERROR of string
    | BINARY_EVENT
    | BINARY_ACK

  val int_of_t : t -> int
  val string_of_t : t -> string

  val event : string -> ?ack:int -> ?namespace:string -> Yojson.Basic.json list -> t
  val ack : int -> ?namespace:string -> Yojson.Basic.json list -> t
end

module Parser : sig
  val decode_packet : string -> Packet.t
  val encode_packet : Packet.t -> string
end

module Socket : sig
  val with_connection : Uri.t -> ?namespace:string -> ((Packet.t Lwt_stream.t) -> (Packet.t -> unit Lwt.t) -> 'a Lwt.t) -> 'a Lwt.t
end
