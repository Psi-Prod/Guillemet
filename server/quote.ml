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
