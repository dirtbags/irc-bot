type t = {sender: string option;
          name: string;
          args: string list;
          text: string option}

let create sender name args text =
  {sender = sender;
   name = name;
   args = args;
   text = text}

let anon = create None

let as_string cmd =
  let ret = Buffer.create 120 in
    (match cmd.sender with
       | None -> 
	   ()
       | Some s ->
	   Buffer.add_char ret ':';
	   Buffer.add_string ret s;
	   Buffer.add_char ret ' ');
    Buffer.add_string ret cmd.name;
    (match cmd.args with
       | [] ->
	   ()
       | l ->
	   Buffer.add_char ret ' ';
	   Buffer.add_string ret (String.concat " " l));
    (match cmd.text with
       | None ->
	   ()
       | Some txt ->
	   Buffer.add_string ret " :";
	   Buffer.add_string ret txt);
    Buffer.contents ret

let extract_word s =
  try
    let pos = String.index s ' ' in
      (Str.string_before s pos, Str.string_after s (pos + 1))
  with Not_found ->
    (s, "")

let rec from_string line =
  (* Very simple.  Pull out words until you get one starting with ":".
     The very first word might start with ":", that doesn't count
     because it's the sender. *)
  let rec loop sender acc line =
    let c = (if (line = "") then None else (Some line.[0])) in
      match (c, acc) with
     	| (None, name :: args) ->
     	    (* End of line, no text part *)
	    create sender (String.uppercase name) args None
	| (None, []) ->
	    (* End of line, no text part, no args, no command *)
	    raise (Failure "No command, eh?")
	| (Some ':', []) ->
	    (* First word, starts with ':' *)
	    let (word, rest) = extract_word line in
	      loop (Some (Str.string_after word 1)) acc rest
	| (Some ':', name :: args) ->
	    (* Not first word, starts with ':' *)
	    create sender (String.uppercase name) args (Some (Str.string_after line 1))
	| (Some _, _) ->
	    (* Argument *)
	    let (word, rest) = extract_word line in
	      loop sender (acc @ [word]) rest
  in
    loop None [] line


let as_tuple cmd = (cmd.sender, cmd.name, cmd.args, cmd.text)

let sender cmd = cmd.sender
let name cmd = cmd.name
let args cmd = cmd.args
let text cmd = cmd.text
