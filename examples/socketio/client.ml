Lwt_log.default :=
  Lwt_log.channel
    ~template:"$(date).$(milliseconds) [$(section)] $(level): $(message)"
    ~close_mode:`Close
    ~channel:Lwt_io.stdout ()

(* let () = Lwt_log.add_rule "*" Lwt_log.Debug *)

open Lwt.Infix

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
  Socketio_client.(
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
  )

let () =
  Lwt_main.run (main ())
