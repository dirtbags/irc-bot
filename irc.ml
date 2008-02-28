type command = {sender: string option;
		command: string;
		args: string list;
		text: string option}

type server = {clients_by_name: (string, client) Hashtbl.t;
	       clients_by_file_descr: (Unix.file_descr, client) Hashtbl.t;
	       channels_by_name: (string, channel) Hashtbl.t}
and client = {outq: string list Queue.t;
	      unsent: string ref;
	      ibuf: string;
	      ibuf_len: int ref;
	      output_ready: unit -> unit;
	      handle_command: server -> client -> command -> unit;
	      channels: channel list}
and channel = {name: string}

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

let extract_word s =
  try
    let pos = String.index s ' ' in
      (Str.string_before s pos, Str.string_after s (pos + 1))
  with Not_found ->
    (s, "")

let string_list_of_command cmd =
  ([] @
     (match cmd.sender with
       | None -> []
       | Some s -> [":" ^ s]) @
     [cmd.command] @
     cmd.args @
     (match cmd.text with
	| None -> []
	| Some s -> [":" ^ s]))

let string_of_command cmd =
  String.concat " " (string_list_of_command cmd)

let rec command_of_string line =
  (* Very simple.  Pull out words until you get one starting with ":".
     The very first word might start with ":", that doesn't count
     because it's the sender.. *)
  let rec loop sender acc line =
    let c = (if (line = "") then None else (Some line.[0])) in
      match (c, acc) with
     	| (None, cmd :: args) ->
     	    (* End of line, no text part *)
	    {sender = sender;
	     command = cmd;
	     args = args;
	     text = None}
	| (None, []) ->
	    (* End of line, no text part, no args, no command *)
	    raise (Failure "No command, eh?")
	| (Some ':', []) ->
	    (* First word, starts with ':' *)
	    let (word, rest) = extract_word line in
	      loop (Some (Str.string_after word 1)) acc rest
	| (Some ':', cmd :: args) ->
	    (* Not first word, starts with ':' *)
	    {sender = sender;
	     command = cmd;
	     args = args;
	     text = Some (Str.string_after line 1)}
	| (Some _, _) ->
	    (* Argument *)
	    let (word, rest) = extract_word line in
	      loop sender (acc @ [word]) rest
  in
    loop None [] line
