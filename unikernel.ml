open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) (R: Mirage_random.C) (KV: Mirage_kv_lwt.RO) = struct

  type content = {
    name : string;
    age  : int;
    buf  : bytes;
  }

  let read_whole_file kv key =
    KV.size kv key >>= function
    | Error e -> Lwt.return @@ Error e
    | Ok size -> KV.read kv key 0L size

  let start s _r kv =
    let port = Key_gen.port () |> int_of_string in
    let _size = Key_gen.size () |> int_of_string in

    let rec callback flow =
      S.TCPV4.read flow >>= function
      | Ok `Eof -> Logs.info (fun f -> f "EOF"); Lwt.return_unit
      | Error e -> Logs.warn
                     (fun f -> f "Error reading data from established connection: %a"
                         S.TCPV4.pp_error e); Lwt.return_unit
      | Ok (`Data data) ->
        let _addr, _port = S.TCPV4.dst flow in
        Logs.info (fun f -> f "cbuf=%d total=%d" (Cstruct.len data) (Marshal.total_size (Cstruct.to_bytes data) 0));
        let out = Marshal.from_string (Cstruct.to_string data) 0 in
        Logs.info (fun f -> f "restoring name=%s age=%d buf=%d" out.name out.age (Bytes.length out.buf));
        callback flow
    in
    S.listen_tcpv4 s ~port:port callback;

    let rec send_data () =
      let localhost = Ipaddr.V4.of_string_exn "127.0.0.1" in
      S.TCPV4.create_connection (S.tcpv4 s) (localhost, port) >>= function
      | Error _err -> OS.Time.sleep_ns (Duration.of_sec 1) >>= fun () -> send_data ()
      | Ok flow ->
        read_whole_file kv "secret" >>= function
        | Error e ->
          Logs.warn (fun f -> f "Could not compare the secret against a known constant: %a"
                        KV.pp_error e); Lwt.return_unit
        | Ok stored_secret ->
          let str =
            Marshal.to_string
              {name="test"; age=9; buf=(Cstruct.to_bytes (Cstruct.concat stored_secret));} []
          in
          let cbuf = Cstruct.of_string str in
          S.TCPV4.write flow cbuf >>= function
          | Error _ -> assert false
          | Ok () ->
            let str = Cstruct.to_string cbuf in
            let orig = Marshal.from_string str 0 in
            Logs.info (fun f -> f "Write out %d bytes name=%s age=%d buf=%d"
                          (Cstruct.len cbuf) orig.name orig.age (Bytes.length orig.buf));
            Lwt.return_unit
    in

    Lwt.join [
      S.listen s;
      send_data ();
    ]

end
