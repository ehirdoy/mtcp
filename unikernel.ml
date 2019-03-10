open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) (R: Mirage_random.C) = struct

  let sflow : S.TCPV4.flow option ref = ref None

  let inc str =
    (String.(get str 0 |> make 1)) ^ str

  let start s _r =
    let _src_addr, dst_addr =
      match List.map Ipaddr.V4.of_string_exn [Key_gen.sa (); Key_gen.da ()] with
      | [sa; da] -> (sa, da)
      | _ -> assert false in
    let src_port, dst_port =
      match List.map int_of_string [Key_gen.sp (); Key_gen.dp ()] with
      | [sp; dp] -> (sp, dp)
      | _ -> assert false in
    let ch = Key_gen.ch () in

    let rec create_sflow count =
      let dst = Ipaddr.V4.to_string dst_addr in
      S.TCPV4.create_connection (S.tcpv4 s) (dst_addr, dst_port) >>= function
      | Error _err ->
        OS.Time.sleep_ns (Duration.of_sec 1) >>= fun () ->
        Logs.debug (fun f -> f "retry(%d) to connect %s:%d" count dst dst_port);
        create_sflow (count+1)
      | Ok flow ->
        Logs.debug (fun f -> f "connected(%d) %s:%d, saving sflow...." count dst dst_port);
        sflow := Some flow;
        Lwt.return_unit
    in

    let rec send str =
      match !sflow with
      | None -> create_sflow 0 >>= fun () -> send str
      | Some fl ->
        S.TCPV4.write fl (Cstruct.of_string str) >>= function
        | Error _ -> assert false
        | Ok () -> Lwt.return_unit
    in

    let rec reply str =
      match !sflow with
      | None ->
        Logs.info (fun f -> f "retry to create sflow");
        OS.Time.sleep_ns (Duration.of_sec 1)
        >>= fun _ -> reply str
      | Some fl ->
        let buf = inc str in
        let addr, port = S.TCPV4.dst fl in
        Logs.debug (fun f -> f "write@%s:%d %dB: %s" (Ipaddr.V4.to_string addr) port (String.length buf) buf);
        S.TCPV4.write fl (Cstruct.of_string buf)
    in

    let rec callback flow =
      S.TCPV4.read flow >>= function

      | Ok `Eof -> Logs.info (fun f -> f "EOF"); Lwt.return_unit
      | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e); Lwt.return_unit
      | Ok (`Data b) ->
        let str = Cstruct.to_string b in
        let addr, port = S.TCPV4.dst flow in
        Logs.debug (fun f -> f "read@%s:%d %dB: %s" (Ipaddr.V4.to_string addr) port (Cstruct.len b) str);
        OS.Time.sleep_ns (Duration.of_ms (Randomconv.int ~bound:3000 (fun x -> R.generate x))) >>= fun () ->
        reply str >>= function
        | Error _ -> assert false
        | Ok () -> callback flow
    in

    let listen () =
      S.listen_tcpv4 s ~port:src_port callback;
      S.listen s;
    in

    Lwt.join [
      send ch;
      listen ();
    ]

end
