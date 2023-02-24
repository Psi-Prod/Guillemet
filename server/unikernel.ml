let src = Logs.Src.create "guillemet"

module Log = (val Logs.src_log src)

module Main (_ : sig end)
(Pclock : Mirage_clock.PCLOCK)
(Stack : Tcpip.Stack.V4V6)
(Time : Mirage_time.S) =
struct
  module Store = Git_kv.Make (Pclock)
  module QotdServer = Server.Make (Stack)
  open Lwt.Syntax
  open Lwt.Infix

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
      QotdServer.run ?port:(Key_gen.port ()) stack
        (serve_qotd store context)
    in
    Lwt.both listen (sync store context)
end
