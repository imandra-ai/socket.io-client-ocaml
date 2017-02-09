module Packet : sig
  type t =
    | CONNECT of string option
    | DISCONNECT
    | EVENT of string * Yojson.Basic.json list * int option * string option
    | ACK of Yojson.Basic.json list * int
    | ERROR of string
    | BINARY_EVENT
    | BINARY_ACK

  val int_of_t : t -> int
  val string_of_t : t -> string

  val event : string -> ?ack:int -> ?namespace:string -> Yojson.Basic.json list -> t
end

module Parser : sig
  val decode_packet : string -> Packet.t
end

module Socket : sig
  val with_connection : Uri.t -> ?namespace:string -> ((Packet.t Lwt_stream.t) -> (Packet.t -> unit Lwt.t) -> 'a Lwt.t) -> 'a Lwt.t
end
