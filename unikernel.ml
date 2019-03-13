open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) (R: Mirage_random.C) = struct

  let start s _r =
    let port = Key_gen.port () |> int_of_string in
    let _size = Key_gen.size () |> int_of_string in

    let rec callback flow =
      S.TCPV4.read flow >>= function
      | Ok `Eof -> Logs.info (fun f -> f "EOF"); Lwt.return_unit
      | Error e -> Logs.warn
                     (fun f -> f "Error reading data from established connection: %a"
                         S.TCPV4.pp_error e); Lwt.return_unit
      | Ok (`Data data) ->
        let str = Cstruct.to_string data in
        let addr, port = S.TCPV4.dst flow in
        Logs.debug (fun f -> f "read@%s:%d %dBytes: %s" (Ipaddr.V4.to_string addr) port (Cstruct.len data) str);
        callback flow
    in
    S.listen_tcpv4 s ~port:port callback;

    let rec send_data () =
      let localhost = Ipaddr.V4.of_string_exn "127.0.0.1" in
      S.TCPV4.create_connection (S.tcpv4 s) (localhost, port) >>= function
      | Error _err -> OS.Time.sleep_ns (Duration.of_sec 1) >>= fun () -> send_data ()
      | Ok flow ->
        Logs.debug (fun f -> f "connected port %d" port);
        S.TCPV4.write flow (Cstruct.of_string "Hello") >>= function
        | Error _ -> assert false
        | Ok () -> Lwt.return_unit
    in

    Lwt.join [
      S.listen s;
      send_data ();
    ]

end
