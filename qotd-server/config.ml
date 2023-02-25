open Mirage

let port =
  let doc =
    Key.Arg.info ~doc:"The port on which to listen for incoming connections."
      [ "port" ]
  in
  Key.(create "port" Arg.(opt (some int) None doc))

let remote =
  let doc =
    Key.Arg.info
      ~doc:
        "Remote repository url, use suffix #foo to specify a branch 'foo': \
         https://github.com/hannesm/unipi.git#gh-pages"
      [ "remote" ]
  in
  Key.(create "remote" Arg.(required string doc))

let ssh_key =
  let doc =
    Key.Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)."
      [ "ssh-key" ]
  in
  Key.(create "ssh-key" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc =
    Key.Arg.info ~doc:"SSH host key authenticator." [ "ssh-authenticator" ]
  in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

let keys = Key.[ v port; v remote; v ssh_key; v ssh_authenticator ]

let main =
  foreign
    ~packages:[ package "git-kv" ]
    ~keys "Unikernel.Main"
    (git_client @-> pclock @-> stackv4v6 @-> time @-> job)

let stack = generic_stackv4v6 default_network

let git_client =
  let dns = generic_dns_client stack in
  let git = git_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  merge_git_clients (git_tcp tcp git)
    (git_ssh ~key:ssh_key ~authenticator:ssh_authenticator tcp git)

let () =
  register "qotd-server"
    [ main $ git_client $ default_posix_clock $ stack $ default_time ]
