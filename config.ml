open Mirage

let daddr =
  let doc = Key.Arg.info ~doc:"destination address" ["daddr"] in
  Key.(create "daddr" Arg.(opt string "127.0.0.1" doc))

let dport =
  let doc = Key.Arg.info ~doc:"The TCP port to send." ["dport"] in
  Key.(create "dport" Arg.(opt string "5555" doc))

let sport =
  let doc = Key.Arg.info ~doc:"The TCP port to send." ["sport"] in
  Key.(create "sport" Arg.(opt string "6666" doc))

let keys = List.map Key.abstract [
    sport;
    daddr;
    dport;
  ]

let main = foreign ~keys "Unikernel.Main" (stackv4 @-> job)

let stack = generic_stackv4 default_network

let () =
  register "inc" [
    main $ stack
  ]
