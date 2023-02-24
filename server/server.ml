let src = Logs.Src.create "guillemet-server"

module Log = (val Logs.src_log src)

module Make (S : Tcpip.Stack.V4V6) = struct
  open Lwt.Infix
  open Lwt.Syntax

  let served quote =
    let len = String.length quote in
    let quote_view =
      if len < 30 then quote
      else String.sub quote 0 30 |> Printf.sprintf "%s [...]"
    in
    Log.info (fun log -> log "Served %S" quote_view)

  let warn_err = function
    | `TCP err ->
        Log.warn (fun log -> log "TCP error: %a" S.TCP.pp_write_error err)
    | `UDP err -> Log.warn (fun log -> log "UDP error: %a" S.UDP.pp_error err)

  let listen_tcp ~on_error ~port stack quote_of_the_day =
    S.TCP.listen stack ~port (fun flow ->
        let* qotd = quote_of_the_day () >|= Quote.to_string in
        let* () =
          Cstruct.of_string qotd |> S.TCP.write flow >|= function
          | Ok () -> served qotd
          | Error err -> on_error (`TCP err)
        in
        S.TCP.close flow)

  let listen_udp ~on_error ~port stack quote_of_the_day =
    S.UDP.listen stack ~port (fun ~src ~dst:_ ~src_port _ ->
        let* qotd = quote_of_the_day () in
        let qotd_str = Quote.to_string qotd in
        Cstruct.of_string qotd_str
        |> S.UDP.write ~src_port:17 ~dst:src ~dst_port:src_port stack
        >|= function
        | Ok () -> served qotd_str
        | Error err -> on_error (`UDP err))

  let run ?(on_error = warn_err) ?(port = 17) stack qotd =
    listen_tcp ~on_error ~port (S.tcp stack) qotd;
    listen_udp ~on_error ~port (S.udp stack) qotd;
    S.listen stack
end
