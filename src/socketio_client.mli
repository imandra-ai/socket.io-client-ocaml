module Packet : sig
  type t =
    | CONNECT
    | DISCONNECT
    | EVENT of string * Yojson.Basic.json list * int option
    | ACK of Yojson.Basic.json list * int
    | ERROR of string
    | BINARY_EVENT
    | BINARY_ACK

  val int_of_t : t -> int
  val string_of_t : t -> string
end

module Socket : sig
  val with_connection : Uri.t -> ((Packet.t Lwt_stream.t) -> (Packet.t -> unit Lwt.t) -> 'a Lwt.t) -> 'a Lwt.t
end
