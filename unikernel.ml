open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) = struct

  let start s =
    let daddr = Key_gen.daddr () |> Ipaddr.V4.of_string_exn in
    let dport = Key_gen.dport () |> int_of_string in
    let sport = Key_gen.sport () |> int_of_string in

    let inc () =
      S.TCPV4.create_connection (S.tcpv4 s) (daddr, dport) >>= function
      | Error _err -> failwith "Connection to port failed"
      | Ok flow ->
        let rec loop n =
          S.TCPV4.write flow (Cstruct.of_string (string_of_int n)) >>= function
          | Error _ -> assert false
          | Ok () -> match n with
            | 9 -> S.TCPV4.close flow
            | _ -> loop (n+1)
        in
        loop 0
    in

    let listen () =
      let rec callback flow =
        let dst, dst_port = S.TCPV4.dst flow in
        Logs.info (fun f -> f "new tcp connection from IP %s on port %d"
                      (Ipaddr.V4.to_string dst) dst_port);
        S.TCPV4.read flow >>= function
        | Ok `Eof -> Logs.info (fun f -> f "Closing connection!"); Lwt.return_unit
        | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
        | Ok (`Data b) ->
          Logs.debug (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b) (Cstruct.to_string b));
          (* S.TCPV4.close flow *)
          callback flow
      in

      S.listen_tcpv4 s ~port:sport callback;
      S.listen s;
    in

    Lwt.join [
      inc ();
      listen ();
    ]

end
