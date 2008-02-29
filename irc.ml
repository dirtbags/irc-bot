let name = ref "irc.test"

let newline_re = Pcre.regexp "\n\r?"
let argsep_re = Pcre.regexp " :"
let space_re = Pcre.regexp " "

let dbg msg a =
  prerr_endline ("[" ^ msg ^ "]");
  a

let string_map f s =
  let l = String.length s in
  if l = 0 then
    s
  else
    let r = String.create l in
      for i = 0 to l - 1 do
	String.unsafe_set r i (f (String.unsafe_get s i))
      done;
      r

let lowercase_char c =
  if (c >= 'A' && c <= '^') then
    Char.unsafe_chr(Char.code c + 32)
  else
    c

let uppercase_char c =
  if (c >= 'a' && c <= '~') then
    Char.unsafe_chr(Char.code c - 32)
  else
    c

let uppercase s = string_map uppercase_char s
let lowercase s = string_map lowercase_char s

let truncate s len =
  let slen = String.length s in
    if len >= slen then
      s
    else
      Str.string_before s (min slen len)
