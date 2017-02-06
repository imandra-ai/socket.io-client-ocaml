Lwt_log.default :=
  Lwt_log.channel
    ~template:"$(date).$(milliseconds) [$(section)] $(level): $(message)"
    ~close_mode:`Close
    ~channel:Lwt_io.stdout ()

let () = Lwt_log.add_rule "*" Lwt_log.Debug

let () =
  Lwt_main.run
    Lwt.(Engineio_client.(
        Lwt_io.printl "running..." >>= fun () ->
        let uri =
          Uri.make
            ~scheme:"http"
            ~host:"localhost"
            (* ~port:3000 *)
            (* ~path:"socket.io/" *)
            ~port:3001
            ~path:"engine.io/"
            ()
        in
        Socket.with_connection uri
          (fun packets send ->
             let rec react_forever () =
               Lwt_stream.get packets >>= function
               | Some (packet_type, _) ->
                 Lwt_io.printlf "-- User got a packet %s!"
                   (Packet.string_of_packet_type packet_type |> String.uppercase_ascii) >>= fun () ->
                 (match packet_type with
                  | Packet.CLOSE -> Lwt.return_unit
                  | _ -> react_forever ())
               | None ->
                 Lwt_io.printl "End of packet stream?" >>= fun () ->
                 Lwt.fail Exit
             in
             let rec sendline () =
               Lwt_io.print "> " >>= fun () ->
               Lwt_io.(read_line_opt stdin) >>= function
               | None -> Lwt_io.printl "Good-bye!"
               | Some content -> send content >>= sendline
             in
             sendline () <?> react_forever ())
      ))
