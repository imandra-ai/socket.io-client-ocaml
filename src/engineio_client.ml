type ready_state =
  | Opening
  | Open
  | Closing
  | Closed

module Util = struct
  module List = struct
    let split_at index list =
      if index <= 0 then
        ([], list)
      else
        let rec loop i t accum =
          if i = 0 then
            (List.rev accum, t)
          else
            match t with
            | [] -> (list, [])
            | hd :: tl -> loop (i - 1) tl (hd :: accum)
        in
        loop index list []
  end

  module Option = struct
    let map ~f = function
      | Some x -> Some (f x)
      | None -> None

    let to_list = function
      | Some x -> [x]
      | None -> []
  end
end

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

  type payload =
    | P_String of string
    | P_Binary of int list

  type t = packet_type * payload

  let string_of_packet_type = function
    | OPEN -> "open"
    | CLOSE -> "close"
    | PING -> "ping"
    | PONG -> "pong"
    | MESSAGE -> "message"
    | UPGRADE -> "upgrade"
    | NOOP -> "noop"
    | ERROR -> "error"

  let packet_type_of_int = function
    | 0 -> OPEN
    | 1 -> CLOSE
    | 2 -> PING
    | 3 -> PONG
    | 4 -> MESSAGE
    | 5 -> UPGRADE
    | 6 -> NOOP
    | _ -> ERROR
end

module Parser = struct

  let protocol = 3

  let decode_packet is_string codes =
    match codes with
    | i :: rest ->
      ( i |> Char.chr |> Stringext.of_char |> int_of_string |> Packet.packet_type_of_int
      , if is_string then
          Packet.P_String
            (List.map Char.chr rest
             |> Stringext.of_list)
        else
          Packet.P_Binary rest
      )
    | [] ->
      (Packet.ERROR, Packet.P_String "Empty packet")

  let decode_payload_as_binary string =
    let decode_payload is_string payload_length codes =
      let (this_packet_data, codes) = Util.List.split_at payload_length codes in
      ( decode_packet is_string this_packet_data
      , codes
      )
    in
    let rec decode_payload_length is_string length = function
      | 255 :: codes ->
        let payload_length =
          length
          |> List.rev_map string_of_int
          |> String.concat ""
          |> int_of_string in
        decode_payload is_string payload_length codes
      | c :: codes -> decode_payload_length is_string (c :: length) codes
      | [] -> raise (Invalid_argument "No payload length")
    in
    let decode_one_packet = function
      | 0 :: codes -> decode_payload_length true [] codes
      | 1 :: codes -> decode_payload_length false [] codes
      | c :: _ -> raise (Invalid_argument (Format.sprintf "Invalid string/binary flag: %i" c))
      | [] -> raise (Invalid_argument "Empty payload")
    in
    let rec go codes =
      match decode_one_packet codes with
      | (packet, []) -> [packet]
      | (packet, codes) -> packet :: go codes
    in
    let char_codes = Stringext.to_list string |> List.map Char.code in
    go char_codes
end

module Transport = struct
  module Polling = struct
    type t =
      { ready_state : ready_state
      ; polling : bool
      ; writeable : bool
      ; uri : Uri.t
      ; packets_out : Packet.t Lwt_stream.t option
      }

    let name = "polling"

    let create uri =
      { ready_state = Closed
      ; polling = false
      ; writeable = false
      ; uri =
          Uri.add_query_param uri ("transport", [name])
      ; packets_out = None
      }

    let poll push_packet t =
      let t = { t with polling = true } in
      Lwt.(
        Cohttp.(Cohttp_lwt_unix.(
            Lwt_io.printlf "Sending request %s" (Uri.to_string t.uri) >>= fun () ->
            Client.get
              ~headers:(Header.init_with "accept" "application/json")
              t.uri >>= fun (resp, body) ->
            let code =
              resp
              |> Response.status
              |> Code.code_of_status in
            Lwt_io.printlf "Received status code: %i" code >>= fun () ->
            if Code.is_success code then
              let log_packet (packet_type, packet_data) =
                Lwt_io.printlf "decoded packet: %s\ndata: '%s'"
                  (Packet.string_of_packet_type packet_type)
                  (match packet_data with
                   | Packet.P_String string -> string
                   | Packet.P_Binary codes -> Format.sprintf "binary packet_data of length %i" (List.length codes))
              in
              let process_packet t (packet_type, packet_data) =
                log_packet (packet_type, packet_data) >>= fun () ->
                let t =
                  match t.ready_state with
                  | Opening -> { t with ready_state = Open; writeable = true }
                  | _ -> t
                in
                let t =
                  (match packet_type with
                   | Packet.CLOSE -> { t with ready_state = Closed }
                   | _ -> t
                  )
                in
                push_packet (Some (packet_type, packet_data));
                return t
              in
              Lwt_stream.fold_s
                (fun line t ->
                   Lwt_io.printlf "Got line '%s'" (String.escaped line) >>= fun () ->
                   let packets = Parser.decode_payload_as_binary line in
                   Lwt_list.fold_left_s process_packet t packets)
                (Cohttp_lwt_body.to_stream body)
                t
            else
              fail_with (Format.sprintf "bad response status: %i" code)
          ))
      )

    let open' t =
      match t.ready_state with
      | Closed ->
        let (stream, push_packet) = Lwt_stream.create () in
        let t =
          { t with
            ready_state = Opening
          ; packets_out = Some stream
          }
        in
        poll push_packet t
      | _ ->
        Lwt.return t
  end

  type t =
    | Polling of Polling.t


  let string_of_t = function
    | Polling _ -> "polling"
end

module Socket = struct
  type t =
    { ready_state : ready_state
    ; transport : Transport.t option
    ; sid : string option
    }

  let make_uri t =
    let query =
      List.append
        [ ("EIO", [string_of_int Parser.protocol]) ]
        (t.sid
         |> Util.Option.map ~f:(fun sid -> ("sid", [sid]))
         |> Util.Option.to_list
        )
    in
    Uri.make
      ~scheme:"http"
      ~host:"localhost"
      ~port:3000
      ~path:"engine.io/"
      ~query
      ()

    let parse_handshake packet_data =
      match packet_data with
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
            (sid, upgrades, ping_interval, ping_timeout)
          | _ -> raise (Invalid_argument "expected an object")
        )

  let open' () =
    let socket =
      { ready_state = Opening
      ; transport = None
      ; sid = None
      } in
    let transport =
      Transport.Polling.create (make_uri socket) in
    let socket =
      { socket with transport = Some (Transport.Polling transport) } in
    Lwt.(
      Transport.Polling.open' transport >>= fun transport ->
      match Transport.Polling.(transport.packets_out) with
      | Some stream ->
        let process_packet (packet_type, packet_data) t =
          match packet_type with
          | Packet.OPEN ->
            let (sid, upgrades, ping_interval, ping_timeout) = parse_handshake packet_data in
            Lwt_io.printlf "Got sid '%s'" sid >>= fun () ->
            return { t with sid = Some sid }
          | _ ->
            return t
        in
        Lwt_stream.fold_s process_packet stream socket
        <?>
        (Lwt_unix.sleep 2.0 >>= fun () ->
         Lwt_io.printl "Timed out." >>= fun () ->
         return socket)
      | None -> return socket
    )
end

let main () =
  Lwt_main.run
    Lwt.(
      Lwt_io.printl "running..." >>= fun () ->
      Socket.open' () >>= fun _ ->
      return_unit
    )
