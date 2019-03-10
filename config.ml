open Mirage

let da =
  let doc = Key.Arg.info ~doc:"destination IP address to send." ["da"] in
  Key.(create "da" Arg.(opt string "127.0.0.1" doc))

let sa =
  let doc = Key.Arg.info ~doc:"source IP address to send." ["sa"] in
  Key.(create "sa" Arg.(opt string "0.0.0.0" doc))

let dp =
  let doc = Key.Arg.info ~doc:"The TCP port to send." ["dp"] in
  Key.(create "dp" Arg.(opt string "5555" doc))

let sp =
  let doc = Key.Arg.info ~doc:"The TCP port to send." ["sp"] in
  Key.(create "sp" Arg.(opt string "6666" doc))

let ch =
  let doc = Key.Arg.info ~doc:"string to send." ["ch"] in
  Key.(create "ch" Arg.(opt string "x" doc))

let keys = List.map Key.abstract [
    sa;
    sp;
    da;
    dp;
    ch;
  ]

let packages = [package "duration"; package "randomconv"]

let main = foreign ~keys ~packages "Unikernel.Main" (stackv4 @-> random @-> job)

let stack = generic_stackv4 default_network

let () =
  register "inc" [
    main $ stack $ default_random
  ]
