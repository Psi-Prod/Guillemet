open Mirage

let port =
  let doc =
    Key.Arg.info
      ~doc:"The TCP port on which to listen for incoming connections."
      [ "port" ]
  in
  Key.(create "port" Arg.(opt (some int) None doc))

let main =
  foreign
    ~keys:Key.[ v port ]
    ~packages:[ package "mehari"; package "mehari-mirage" ]
    "Unikernel.GeminiServer"
    (random @-> pclock @-> stackv4v6 @-> time @-> job)

let () =
  register "gemini-srv"
    [
      main $ default_random $ static $ certs $ default_posix_clock
      $ generic_stackv4v6 default_network
      $ default_time;
    ]
