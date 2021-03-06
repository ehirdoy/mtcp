open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) (R: Mirage_random.C) (KV: Mirage_kv_lwt.RO) = struct

  type content = {
    name : string;
    age  : int;
    buf  : bytes;
  }

  module Pbuf = struct

    type t = {
      len : int;
      bufs: bytes list;
    }

    let cxt = ref {
        len = 0;
        bufs = [];
      }

    let add bytes =
      if !cxt.len = 0 then
        cxt := {
          len = Marshal.total_size bytes 0;
          bufs = bytes :: !cxt.bufs;
        }
      else
        cxt := {
          len = !cxt.len;
          bufs = bytes :: !cxt.bufs;
        }

    let is_filled () =
      !cxt.len = List.fold_left (fun a el -> a + Bytes.length el) 0 !cxt.bufs

    let bufs () =
      Logs.info (fun f -> f "concatinating %d bufs" (List.length !cxt.bufs));
      let bytes = Bytes.concat Bytes.empty (List.rev !cxt.bufs) in
      cxt := { len = 0; bufs = []; };
      bytes

    let restore () =
      let out = Marshal.from_string (Bytes.to_string (bufs ())) 0 in
      Logs.info (fun f -> f "restoring name=%s age=%d buf=%d" out.name out.age (Bytes.length out.buf))

  end

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
        let bytes = Cstruct.to_bytes data in
        Pbuf.add bytes;
        if Pbuf.is_filled () then
          Pbuf.restore ();
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
