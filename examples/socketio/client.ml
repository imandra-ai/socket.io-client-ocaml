Lwt_log.default :=
  Lwt_log.channel
    ~template:"$(date).$(milliseconds) [$(section)] $(level): $(message)"
    ~close_mode:`Close
    ~channel:Lwt_io.stdout ()

(* let () = Lwt_log.add_rule "*" Lwt_log.Debug *)

open Lwt.Infix

module Util = struct
  module Char = struct
    let is_digit = function
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
      | _ -> false
  end

  module Option = struct
    let map : f:('a -> 'b) -> 'a option -> 'b option =
      fun ~f ->
        function
        | Some x -> Some (f x)
        | None -> None

    let value : default:'a -> 'a option -> 'a =
      fun ~default ->
        function
        | Some x -> x
        | None -> default

    let value_map : f:('a -> 'b) -> default:'b -> 'a option -> 'b =
      fun ~f ~default t ->
        map ~f t |> value ~default
  end

  module String = struct
    let uncons : string -> (char * string) option =
      fun string ->
        let len = String.length string in
        if len = 0 then
          None
        else
          Some (String.get string 0, String.sub string 1 (len - 1))

    let split_at : int -> string -> (string * string) option =
      fun i string ->
        let len = String.length string in
        if i > len then
          None
        else
          Some (String.sub string 0 i, String.sub string i (len - i))
  end
end

module Packet = struct
  type t =
    | CONNECT
    | DISCONNECT
    | EVENT of string * Yojson.Basic.json list * int option
    | ACK of Yojson.Basic.json list * int
    | ERROR of string
    | BINARY_EVENT
    | BINARY_ACK

  let int_of_t : t -> int =
    function
    | CONNECT -> 0
    | DISCONNECT -> 1
    | EVENT _ -> 2
    | ACK _ -> 3
    | ERROR _ -> 4
    | BINARY_EVENT -> 5
    | BINARY_ACK -> 6

  let string_of_t : t -> string =
    function
    | CONNECT -> "CONNECT"
    | DISCONNECT -> "DISCONNECT"
    | EVENT (name, _, _) -> Printf.sprintf "EVENT[%s]" name
    | ACK (_, ack_id)-> Printf.sprintf "ACK[%i]" ack_id
    | ERROR _ -> "ERROR"
    | BINARY_EVENT -> "BINARY_EVENT"
    | BINARY_ACK -> "BINARY_ACK"
end

module Parser = struct
  let rec decode_ack data ack_chars_rev =
    match Util.String.uncons data with
    | Some ('[', rest) ->
      let ack =
        match ack_chars_rev with
        | [] -> None
        | _ ->
          Some
            (ack_chars_rev
             |> List.rev
             |> Stringext.of_list
             |> int_of_string)
      in
      (ack, data)
    | Some (c, rest) -> decode_ack rest (c :: ack_chars_rev)
    | None -> (None, data)

  let decode_packet_event : string -> Packet.t =
    fun data ->
      let ack, rest = decode_ack data [] in
      let json =
        try Ok (Yojson.Basic.from_string rest) with
        | Yojson.Json_error msg -> Error msg
      in
      match json with
      | Ok (`List (`String event_name :: arguments)) ->
        Packet.EVENT (event_name, arguments, ack)
      | Ok _ -> Packet.ERROR "Expected a list in the packet data."
      | Error msg -> Packet.ERROR (Printf.sprintf "JSON error: %s" msg)

  let decode_packet_ack : string -> Packet.t =
    fun data ->
      let ack, rest = decode_ack data [] in
      match ack with
      | None -> Packet.ERROR (Printf.sprintf "Ack packet with no id: %s" data)
      | Some ack_id ->
        let json =
          try Ok (Yojson.Basic.from_string rest) with
          | Yojson.Json_error msg -> Error msg
        in
        (match json with
         | Ok (`List arguments) ->
           Packet.ACK (arguments, ack_id)
         | Ok _ -> Packet.ERROR "Expected an argument list in the packet data."
         | Error msg -> Packet.ERROR (Printf.sprintf "JSON error: %s" msg))


  let decode_packet : string -> Packet.t =
    fun data ->
      match Util.String.uncons data with
      | None -> Packet.ERROR "no data in packet"
      | Some ('0', "") -> Packet.CONNECT
      | Some ('1', "") -> Packet.DISCONNECT
      | Some ('2', event_data) -> decode_packet_event event_data
      | Some ('3', ack_data) -> decode_packet_ack ack_data
      | Some ('4', error) -> Packet.ERROR error
      | Some ('5', "") -> Packet.BINARY_EVENT
      | Some ('6', "") -> Packet.BINARY_ACK
      | _ -> Packet.ERROR (Printf.sprintf "Could not decode packet data: %s" data)

  let encode_packet : Packet.t -> string =
    fun packet ->
    match packet with
    | Packet.EVENT (event_name, data, ack) ->
      Printf.sprintf "%i%s%s"
        (Packet.int_of_t packet)
        (Util.Option.value_map ~default:"" ~f:string_of_int ack)
        (Yojson.Basic.to_string (`List (`String event_name :: data)))
    | Packet.ACK (data, ack_id) ->
      Printf.sprintf "%i%i%s"
        (Packet.int_of_t packet)
        ack_id
        (Yojson.Basic.to_string (`List data))
    | _ ->
      "not implemented"
end

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
    Lwt_log.info_f ~section "on_packet %s" (Packet.string_of_t packet) >>= fun () ->
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
    let packet = Parser.decode_packet data  in
    on_packet socket packet
    |> Lwt.map (fun socket -> (socket, packet))

  let process_eio_packet : t * Packet.t list -> Engineio_client.Packet.t -> (t * Packet.t list) Lwt.t =
    fun (socket, packets_received_rev) (packet_type, packet_data) ->
      Lwt_log.info_f ~section "on_eio_packet %s" (Engineio_client.Packet.string_of_packet_type packet_type) >>= fun () ->
      match packet_type with
      | Engineio_client.Packet.OPEN ->
        on_open socket
        |> Lwt.map (fun socket -> (socket, packets_received_rev))
      | Engineio_client.Packet.MESSAGE ->
        on_message socket packet_data >>= fun (socket, packet_received) ->
        Lwt.return (socket, packet_received :: packets_received_rev)
      | _ -> Lwt.return (socket, packets_received_rev)

  (* Entry point *)

  let with_connection : Uri.t -> ((Packet.t Lwt_stream.t) -> (Packet.t -> unit Lwt.t) -> 'a Lwt.t) -> 'a Lwt.t =
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
      let sleep_until_packet_to_send () =
        Lwt_stream.peek packets_send_stream >>= fun _ ->
        Lwt_log.info ~section "Waking to send a packet"
      in
      let sleep_until_packet_received packet_stream =
        Lwt_stream.peek packet_stream >>= fun _ ->
        Lwt_log.info ~section "Waking to process a packet"
      in
      Engineio_client.Socket.with_connection uri
        (fun packet_stream send_message ->
           let rec react_forever socket =
             Lwt.choose
               [ sleep_until_packet_to_send ()
               ; sleep_until_packet_received packet_stream
               ] >>= fun () ->
             packet_stream
           |> Lwt_stream.get_available
           |> Lwt_list.fold_left_s process_eio_packet (socket, [])
           >>= fun (socket, packets_rev) ->
           let () =
             packets_rev
             |> List.rev
             |> List.iter (fun packet -> push_packet_recv (Some packet))
           in
           packets_send_stream
           |> Lwt_stream.get_available
           |> Lwt_list.iter_s
             (fun packet ->
                Parser.encode_packet packet
                |> send_message) >>= fun () ->
           react_forever socket
           in
           Lwt.choose
             [ react_forever socket
             ; user_promise >>= fun _ -> Lwt.return_unit
             ] >>= fun () ->
           user_promise
        )
end

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
         | Some packet ->
           Lwt_io.printlf "-- User got a packet! %s"
             (Packet.string_of_t packet) >>= fun () ->
           (match packet with
            | Packet.EVENT ("chat message", [`String content], _) ->
              Lwt_io.printlf "got message: %s" content
            | Packet.EVENT ("pls respond", [`String content], Some ack_id) ->
              Lwt_io.printlf "responding to ack request %i" ack_id >>= fun () ->
              send (Packet.ACK ([`String (Printf.sprintf "OCaml is replying to your message: %s" content)], ack_id))
            | Packet.ERROR error ->
              Lwt_io.printlf "got error: %s" error
            | _ -> Lwt.return_unit) >>= react_forever
         | None ->
           Lwt_io.printl "End of packet stream?" >>= fun () ->
           Lwt.fail Exit
       in
       let rec sendline () =
         Lwt_io.(read_line_opt stdin) >>= function
         | None -> Lwt_io.printl "Good-bye!"
         | Some content -> send (Packet.EVENT ("chat message", [`String content], None)) >>= sendline
       in
       sendline () <?> react_forever ())

let () =
  Lwt_main.run (main ())
