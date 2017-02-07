Lwt_log.default :=
  Lwt_log.channel
    ~template:"$(date).$(milliseconds) [$(section)] $(level): $(message)"
    ~close_mode:`Close
    ~channel:Lwt_io.stdout ()

let () = Lwt_log.add_rule "*" Lwt_log.Debug

open Lwt.Infix

module Packet = struct
  type packet_type =
    | CONNECT
    | DISCONNECT
    | EVENT
    | ACK
    | ERROR
    | BINARY_EVENT
    | BINARY_ACK

  type t = packet_type

  let int_of_packet_type : packet_type -> int =
    function
    | CONNECT -> 0
    | DISCONNECT -> 1
    | EVENT -> 2
    | ACK -> 3
    | ERROR -> 4
    | BINARY_EVENT -> 5
    | BINARY_ACK -> 6

  let packet_type_of_int : int -> packet_type option =
    function
    | 0 -> Some CONNECT
    | 1 -> Some DISCONNECT
    | 2 -> Some EVENT
    | 3 -> Some ACK
    | 4 -> Some ERROR
    | 5 -> Some BINARY_EVENT
    | 6 -> Some BINARY_ACK
    | _ -> None

  let string_of_packet_type : packet_type -> string =
    function
    | CONNECT -> "CONNECT"
    | DISCONNECT -> "DISCONNECT"
    | EVENT -> "EVENT"
    | ACK -> "ACK"
    | ERROR -> "ERROR"
    | BINARY_EVENT -> "BINARY_EVENT"
    | BINARY_ACK -> "BINARY_ACK"
end

module Parser = struct
  let decode_packet : string -> Packet.t option =
    fun string ->
      match string with
      | "0" -> Some Packet.CONNECT
      | _ -> None
end

(* module Event = struct *)
(*   type t = *)
(*     | Connect *)
(*     | Connect_error *)
(*     | Connect_timeout *)
(*     | Connecting *)
(*     | Disconnect *)
(*     | Error *)
(*     | Reconnect *)
(*     | Reconnect_attempt *)
(*     | Reconnect_failed *)
(*     | Reconnect_error *)
(*     | Reconnecting *)
(*     | Ping *)
(*     | Pong *)
(* end *)

module Socket = struct
  let section =
    Lwt_log.Section.make "socketio.socket"

  type t =
    { uri : Uri.t
    ; connected : bool
    ; ids : int
    ; nsp : string
    ; acks : int list
    }

  let create : Uri.t -> t =
    fun uri ->
      { uri = uri
      ; connected = false
      ; ids = 0
      ; nsp = "/"
      ; acks = []
      }

  (* Socket.io packet handlers *)

  let on_connect socket =
    Lwt.return
      { socket with
        connected = true
      }

  let on_packet socket packet =
    Lwt_log.info_f ~section "on_packet %s" (Packet.string_of_packet_type packet) >>= fun () ->
    match packet with
    | Packet.CONNECT -> on_connect socket
    | _ -> Lwt.return socket

  (* Engine.io packet handlers *)

  let on_open socket =
    if socket.nsp <> "/" then
      (* TODO: send a Connect packet *)
      Lwt.return socket
    else
      Lwt.return socket

  let on_message socket packet_data =
    let data = Engineio_client.Packet.string_of_packet_data packet_data in
    match Parser.decode_packet data with
    | None ->
      Lwt_log.error_f ~section "Could not decode packet '%s'" data >>= fun () ->
      Lwt.return socket
    | Some packet -> on_packet socket packet

  let process_eio_packet socket (packet_type, packet_data) =
    Lwt_log.info_f ~section "on_eio_packet %s" (Engineio_client.Packet.string_of_packet_type packet_type) >>= fun () ->
    match packet_type with
    | Engineio_client.Packet.OPEN -> on_open socket
    | Engineio_client.Packet.MESSAGE -> on_message socket packet_data
    | _ -> Lwt.return socket

  (* Entry point *)

  let with_connection : Uri.t -> ((Packet.t Lwt_stream.t) -> (string -> unit Lwt.t) -> 'a Lwt.t) -> 'a Lwt.t =
    fun uri f ->
      (* packets recieved *)
      let (packets_recv_stream, push_packet_recv) =
        Lwt_stream.create () in
      (* packets to send *)
      let (packets_send_stream, push_packet_send) =
        Lwt_stream.create () in
      let send packet =
        push_packet_send (Some packet); Lwt.return_unit in
      let user_promise =
        f packets_recv_stream send in
      let socket = create uri in
      Engineio_client.Socket.with_connection uri
        Engineio_client.(fun packet_stream send_message ->
            let rec react_forever () =
              Lwt_stream.get packet_stream >>= (function
                  | None -> Lwt_log.error ~section "End of packet_stream"
                  | Some packet ->
                    let socket = process_eio_packet socket packet in
                    Lwt.return_unit)
              >>= react_forever
            in
            Lwt.choose
              [ react_forever ()
              ; user_promise >>= fun _ -> Lwt.return_unit
              ] >>= fun () ->
            user_promise
          )
end

(* module Manager = struct *)
(*   type ready_state = *)
(*     | Closed *)
(*     | Opening *)
(*     | Open *)

(*   module String_map = Map.Make(String) *)

(*   type t = *)
(*     { uri : Uri.t *)
(*     ; ready_state : ready_state *)
(*     ; nsps : Socket.t String_map.t *)
(*     } *)

(*   let create : Uri.t -> t = *)
(*     fun uri -> *)
(*       { uri = uri *)
(*       ; ready_state = Closed *)
(*       ; nsps = String_map.empty *)
(*       } *)
(* end *)

let main () =
  Lwt_io.printl "Starting..." >>= fun () ->
  let uri =
    Uri.make
      ~scheme:"http"
      ~host:"localhost"
      ~port:3000
      ~path:"socket.io/"
      ()
  in
  Socket.with_connection uri
    (fun packets send ->
       let rec react_forever () =
         Lwt_stream.get packets >>= function
         | Some packet_type ->
           Lwt_io.printl "-- User got a packet!" >>= react_forever
         | None ->
           Lwt_io.printl "End of packet stream?" >>= fun () ->
           Lwt.fail Exit
       in
       let rec sendline () =
         Lwt_io.(read_line_opt stdin) >>= function
         | None -> Lwt_io.printl "Good-bye!"
         | Some content -> send content >>= sendline
       in
       sendline () <?> react_forever ())

let () =
  Lwt_main.run (main ())
