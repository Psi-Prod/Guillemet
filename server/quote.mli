type t = private string

val make :
  ?non_ascii:[ `Enable | `Ignore | `Raise ] ->
  ?size_exceed:[ `Raise | `Truncate ] ->
  string ->
  t

val to_string : t -> string
