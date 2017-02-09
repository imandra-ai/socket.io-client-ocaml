open Lwt.Infix

(* Still to implement:

   - Re-connection, back-off, etc.
   - Multiplexing: Connecting to and disconnecting from namespaces.
   - Binary events/acks.
*)

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

    let any_digit =
      satisfy (function '0' .. '9' -> true | _ -> false) >>| fun c -> int_of_string (Stringext.of_char c)

    let any_integer =
      take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

    let any_string_until p =
      many_till any_char p >>| Stringext.of_list

    let json_until_end_of_input : Yojson.Basic.json Angstrom.t =
      any_string_until end_of_input >>= fun arg_string ->
      (try return (Yojson.Basic.from_string arg_string) with
       | Yojson.Json_error msg -> fail msg)

    let packet_connect : Packet.t Angstrom.t =
      option None (many1 any_char >>| fun chars -> Some (Stringext.of_list chars))
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
      | Error message -> Packet.ERROR (Printf.sprintf "Error decoding packet: %s" message)

  let encode_packet : Packet.t -> string =
    fun packet ->
      match packet with
      | Packet.CONNECT namespace ->
        Printf.sprintf "%i%s"
          (Packet.int_of_t packet)
          (Util.Option.value ~default:"" namespace)
      | Packet.DISCONNECT ->
        Printf.sprintf "%i"
          (Packet.int_of_t packet)
      | Packet.EVENT (event_name, data, ack, nsp) ->
        Printf.sprintf "%i%s%s%s"
          (Packet.int_of_t packet)
          (nsp
           |> Util.Option.value_map ~default:""
             ~f:(fun nsp -> Printf.sprintf "%s," nsp))
          (ack
           |> Util.Option.value_map ~default:""
             ~f:string_of_int)
          (Yojson.Basic.to_string (`List (`String event_name :: data)))
      | Packet.ACK (data, ack_id, namespace) ->
        Printf.sprintf "%i%s%i%s"
          (Packet.int_of_t packet)
          (namespace
           |> Util.Option.value_map ~default:""
             ~f:(fun nsp -> Printf.sprintf "%s," nsp))
          ack_id
          (Yojson.Basic.to_string (`List data))
      | _ ->
        raise (Invalid_argument (Printf.sprintf "Encoding %s: not implemented" (Packet.string_of_t packet)))
end

module Socket = struct
  let section =
    Lwt_log.Section.make "socketio.socket"

  type t =
    { uri : Uri.t
    ; connected : bool
    ; ids : int
    ; namespace : string option
    (* None means the default namespace, "/" *)
    ; acks : int list
    }

  let create : Uri.t -> string option -> t =
    fun uri namespace ->
      { uri = uri
      ; connected = false
      ; ids = 0
      ; namespace = namespace
      ; acks = []
      }

  (* Socket.io packet handlers *)

  let on_connect socket namespace =
    Lwt.return
      { socket with
        connected =
          namespace = socket.namespace
      }

  let on_packet socket packet =
    Lwt_log.info_f ~section "on_packet %s" (Packet.string_of_t packet) >>= fun () ->
    match packet with
    | Packet.CONNECT namespace -> on_connect socket namespace
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

  let process_eio_packet : t * Packet.t list * Packet.t list -> Engineio_client.Packet.t -> (t * Packet.t list * Packet.t list) Lwt.t =
    fun (socket, packets_received_rev, packets_to_send) (packet_type, packet_data) ->
      Lwt_log.info_f ~section "on_eio_packet %s" (Engineio_client.Packet.string_of_packet_type packet_type) >>= fun () ->
      match packet_type with
      | Engineio_client.Packet.OPEN ->
        on_open socket
        |> Lwt.map (fun (socket, open_packets_to_send) -> (socket, packets_received_rev, List.append packets_to_send open_packets_to_send))
      | Engineio_client.Packet.MESSAGE ->
        on_message socket packet_data >>= fun (socket, packet_received) ->
        Lwt.return (socket, packet_received :: packets_received_rev, packets_to_send)
      | _ -> Lwt.return (socket, packets_received_rev, packets_to_send)

  (* Entry point *)

  let with_connection : Uri.t -> ?namespace:string -> ((Packet.t Lwt_stream.t) -> (Packet.t -> unit Lwt.t) -> 'a Lwt.t) -> 'a Lwt.t =
    fun uri ?namespace f ->
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
        (fun packet_stream send_message ->
           let flush_user_packets socket =
             if socket.connected then
               packets_send_stream
               |> Lwt_stream.get_available
               |> Lwt_list.iter_s
                 (fun packet ->
                    packet
                    |> Packet.with_namespace ~nsp:socket.namespace
                    |> Parser.encode_packet
                    |> send_message)
             else
               Lwt_log.debug "Socket not connected: not flushing."
           in
           let rec react_forever socket =
             Lwt.choose
               (List.concat
                  [ if socket.connected then [sleep_until_packet_to_send ()] else []
                  ; [ sleep_until_packet_received packet_stream ]
                  ]) >>= fun () ->
             packet_stream
             |> Lwt_stream.get_available
             |> Lwt_list.fold_left_s process_eio_packet (socket, [], [])
             >>= fun (socket, packets_received_rev, packets_to_send) ->
             let () =
               packets_received_rev
               |> List.rev
               |> List.iter (fun packet -> push_packet_recv (Some packet))
             in
             packets_to_send
             |> Lwt_list.iter_s
               (fun packet ->
                  Parser.encode_packet packet |> send_message) >>= fun () ->
             flush_user_packets socket >>= fun () ->
             react_forever socket
           in
           Lwt.choose
             [ react_forever socket
             ; user_promise >>= fun _ -> Lwt.return_unit
             ] >>= fun () ->
           Packet.DISCONNECT |> Parser.encode_packet |> send_message >>= fun () ->
           user_promise
        )
end
