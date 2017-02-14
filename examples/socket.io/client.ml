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
    Socket.with_connection uri ~namespace:"/a-namespace"
      (fun packets send ->
         let rec react_forever next_ack_id =
           Lwt_stream.get packets >>= function
           | Some packet ->
             Lwt_io.printlf "-- User got a packet! %s"
               (Packet.string_of_t packet) >>= fun () ->

             (match packet with
              | Packet.EVENT ("chat message", [`String content], _, _) ->
                Lwt_io.printlf "got message: %s" content >>= fun () ->
                Lwt.return next_ack_id

              | Packet.EVENT ("pls respond", [`String content], Some ack_id, _) ->
                Lwt_io.printlf "responding to ack request %i" ack_id >>= fun () ->
                send (Packet.ack ack_id [`String (Printf.sprintf "OCaml is replying to your message: %s" content)]) >>= fun () ->

                Lwt_io.printlf "sending event 'pls respond too' with ack %i" next_ack_id >>= fun () ->
                send (Packet.event "pls respond too" [`String "I'm needy too"] ~ack:next_ack_id) >>= fun () ->
                Lwt.return (next_ack_id + 1)

              | Packet.ERROR error ->
                Lwt_io.printlf "got error: %s" error >>= fun () ->
                Lwt.return next_ack_id

              | _ -> Lwt.return next_ack_id)

             >>= react_forever

           | None ->
             Lwt_io.printl "End of packet stream?" >>= fun () ->
             Lwt.fail Exit
         in

         let rec sendline () =
           Lwt_io.(read_line_opt stdin) >>= function
           | None -> Lwt_io.printl "Good-bye!"
           | Some content -> send (Packet.event "chat message" [`String content]) >>= sendline
         in

         sendline () <?> react_forever 0)
  )

let () =
  Lwt_main.run (main ())
