open Mirage

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
    sp;
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
