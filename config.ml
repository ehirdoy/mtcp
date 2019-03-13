open Mirage

let port =
  let doc = Key.Arg.info ~doc:"Port to send & receive" ["port"] in
  Key.(create "port" Arg.(opt string "6666" doc))

let size =
  let doc = Key.Arg.info ~doc:"size of data" ["size"] in
  Key.(create "size" Arg.(opt string "4096" doc))

let keys = List.map Key.abstract [
    port;
    size;
  ]

let packages = [package "duration"; package "randomconv"]

let main = foreign ~keys ~packages "Unikernel.Main" (stackv4 @-> random @-> job)

let stack = generic_stackv4 default_network

let () =
  register "size" [
    main $ stack $ default_random
  ]
