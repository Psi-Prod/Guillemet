module Quote : sig
  type t = private string

  val make :
    ?non_ascii:[ `Enable | `Ignore | `Raise ] ->
    ?size_exceed:[ `Raise | `Truncate ] ->
    string ->
    t

  val to_string : t -> string
end = struct
  type t = string

  let print strategy s =
    let buf = Buffer.create 512 in
    String.iter
      (fun chr ->
        match (chr, strategy) with
        | c, `Enable -> Buffer.add_char buf c
        | ((' ' .. '~' | '\r' | '\n') as c), _ -> Buffer.add_char buf c
        | _, `Ignore -> ()
        | _, `Raise -> invalid_arg "print")
      s;
    Buffer.contents buf

  let make ?(non_ascii = `Ignore) ?(size_exceed = `Truncate) s =
    let len = String.length s in
    let quote =
      match size_exceed with
      | `Raise when len > 512 -> invalid_arg "Quote.make"
      | `Truncate when len > 512 -> String.sub s 0 512
      | `Raise | `Truncate -> s
    in
    print non_ascii quote

  let to_string s = s
end

module QotdServerMake (S : Tcpip.Stack.V4V6) : sig
  val run :
    ?on_error:([ `TCP of S.TCP.write_error | `UDP of S.UDP.error ] -> unit) ->
    ?port:int ->
    S.t ->
    (unit -> Quote.t Lwt.t) ->
    unit Lwt.t
end = struct
  open Lwt.Infix
  open Lwt.Syntax

  let src = Logs.Src.create "guillemet-server"

  module Log = (val Logs.src_log src)

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

module Main (_ : sig end)
(Pclock : Mirage_clock.PCLOCK)
(Stack : Tcpip.Stack.V4V6)
(Time : Mirage_time.S) =
struct
  module Store = Git_kv.Make (Pclock)
  module QotdServer = QotdServerMake (Stack)
  open Lwt.Syntax
  open Lwt.Infix

  let src = Logs.Src.create "guillemet"

  module Log = (val Logs.src_log src)

  type t = { mutable quotes : Store.key Queue.t; mutable today : Ptime.date }

  let create () =
    {
      quotes = Queue.create ();
      today = Pclock.now_d_ps () |> Ptime.unsafe_of_d_ps |> Ptime.to_date;
    }

  let quote_or_empty store key =
    Store.get store key >|= function
    | Ok quote -> quote
    | Error err ->
        Log.warn (fun l -> l "%a" Store.pp_error err);
        ""

  let cycle store queue =
    match Queue.take_opt queue with
    | None ->
        Log.info (fun log ->
            log "Quotes queue is empty, an empty quote will be served");
        Lwt.return ""
    | Some key ->
        Queue.push key queue;
        quote_or_empty store key

  let serve_qotd store ctx () =
    let now = Pclock.now_d_ps () |> Ptime.unsafe_of_d_ps |> Ptime.to_date in
    let+ quote =
      if ctx.today < now then (
        ctx.today <- now;
        cycle store ctx.quotes)
      else Queue.top ctx.quotes |> quote_or_empty store
    in
    Quote.make ~non_ascii:`Enable quote

  let fill_queue store ctx =
    Store.list store Mirage_kv.Key.empty >|= function
    | Ok quotes ->
        List.iter
          (fun (key, typ) ->
            match typ with
            | `Value -> Queue.push key ctx.quotes
            | `Dictionary -> ())
          quotes
    | Error err -> Log.warn (fun l -> l "%a" Store.pp_error err)

  let perform_changes store quotes changes =
    let push_value key queue =
      Store.exists store key >|= function
      | Ok (Some `Value) -> Queue.push key queue
      | Ok (Some `Dictionary) | _ -> ()
    in
    Lwt_list.fold_left_s
      (fun queue -> function
        | `Add key ->
            let+ () = push_value key queue in
            queue
        | `Change key ->
            Queue.fold
              (fun queue quote ->
                let* queue = queue in
                let+ () =
                  if Mirage_kv.Key.equal quote key then push_value key queue
                  else push_value quote queue
                in
                queue)
              (Lwt.return (Queue.create ()))
              queue
        | `Remove key ->
            Queue.fold
              (fun queue quote ->
                let* queue = queue in
                let+ () =
                  if Mirage_kv.Key.equal quote key then Lwt.return_unit
                  else push_value quote queue
                in
                queue)
              (Lwt.return (Queue.create ()))
              queue)
      quotes changes

  let rec sync store ctx =
    Log.info (fun log -> log "Start to pull repository");
    let* () =
      Git_kv.pull store >>= function
      | Ok changes ->
          let+ queue = perform_changes store ctx.quotes changes in
          ctx.quotes <- queue
      | Error (`Msg msg) ->
          Log.warn (fun log -> log "Error while pulling repo: %s" msg);
          Lwt.return_unit
    in
    let* () = Time.sleep_ns (Duration.of_hour 12) in
    sync store ctx

  let start git_ctx _clock stack _default_time =
    let* store = Git_kv.connect git_ctx (Key_gen.remote ()) in
    let context = create () in
    let* () = fill_queue store context in
    let listen =
      QotdServer.run ?port:(Key_gen.port ()) stack (serve_qotd store context)
    in
    Lwt.both listen (sync store context)
end
