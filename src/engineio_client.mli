module Packet : sig
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
    | P_Binary of int list

  type t = packet_type * packet_data

  val string_of_packet_type : packet_type -> string
  val packet_type_of_int : int -> packet_type
  val int_of_packet_type : packet_type -> int
end

module Socket : sig
  val with_connection :  Uri.t -> ((Packet.t Lwt_stream.t) -> (string -> unit Lwt.t) -> 'a Lwt.t) -> 'a Lwt.t
end
