module Make (S : Tcpip.Stack.V4V6) : sig
  val run :
    ?on_error:([ `TCP of S.TCP.write_error | `UDP of S.UDP.error ] -> unit) ->
    ?port:int ->
    S.t ->
    (unit -> Quote.t Lwt.t) ->
    unit Lwt.t
end
