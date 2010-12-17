let debug = prerr_endline

let file_descr_of_int (i:int) =
  let blob = Marshal.to_string i [] in
    (Marshal.from_string blob 0 : Unix.file_descr)

let write iobuf command args text =
  let cmd = Command.create None command args text in
    Iobuf.write iobuf cmd

let rec msg iobuf recip text =
  match text with
    | "" -> ()
    | _ ->
      let tl = String.length text in
      let s, rest = 
        if (tl > 400) then
          ((Str.string_before text 400) ^ "↩",
           "↪" ^ (Str.string_after text 400))
        else
          (text, "")
      in
        write iobuf "PRIVMSG" [recip] (Some s);
        msg iobuf recip rest

let split = Str.split (Str.regexp "[ \t]*\r?\n")

(** Callback upon completion of the external command helper *)
let extern_callback iobuf sender forum text =
  let lines = split text in
  let nlines = List.length lines in
  let recip =
    if (nlines > 5) then begin
      if (forum <> sender) then
        msg iobuf forum (Format.sprintf "%d lines, sending privately" nlines);
      sender
    end else
      forum
  in
  let rec f = function
    | [] ->
        ()
    | "" :: tl ->
        f tl
    | line :: tl ->
        if line.[0] == '\007' then
          (* Interpret as raw IRC commands *)
          let ine = Str.string_after line 1 in
          let cmd = Command.from_string ine in
            Iobuf.write iobuf cmd
        else
          (* Naive case: send to the recipient *)
          msg iobuf recip line;
        f tl
  in
    f lines

let nick_of_nuhost s =
  try
    Irc.nick (Irc.nuhost_of_string s)
  with Not_found ->
    s
    
let handle_command outbuf handle_cmd thisbuf cmd =
  let (prefix, command, args, trailing) = Command.as_tuple cmd in
  let (sender, forum) =
    match (prefix, command, args, trailing) with
      | (Some suhost, "PRIVMSG", [target], _)
      | (Some suhost, "NOTICE", [target], _) ->
          let sender = nick_of_nuhost suhost in
          let forum = if Irc.is_channel target then target else sender in
            (sender, forum)

(* Here's why the IRC protocol blows: *)
      | (Some suhost, "PART", [forum], _)
      | (Some suhost, "JOIN", [forum], _)
      | (Some suhost, "MODE", forum :: _, _)
      | (Some suhost, "INVITE", [_; forum], None)
      | (Some suhost, "INVITE", _, Some forum)
      | (Some suhost, "TOPIC", forum :: _, _)
      | (Some suhost, "KICK", forum :: _, _) ->
          (nick_of_nuhost suhost, forum)

      | (Some suhost, "JOIN", [], Some chan) ->
          (nick_of_nuhost suhost, chan)

      | (Some _, "NICK", [sender], _) ->
          (sender, sender)

      | (Some suhost, "QUIT", _, _)
      | (Some suhost, _, _, _) ->
          let sender = nick_of_nuhost suhost in
            (sender, sender)

      | (_, "PING", _, Some text) ->
          write outbuf "PONG" [] (Some text);
          ("", "")

      | (None, _, _, _) ->
          ("", "")
  in
  let pfx =
    match prefix with
      | Some txt -> txt
      | None -> ""
  in
  let text =
    match trailing with
      | Some txt -> txt
      | None -> ""
  in
  let argv =
    Array.append
      [|handle_cmd; sender; forum; pfx; command|]
      (Array.of_list args)
  in
    Process.create_canned
      (Iobuf.dispatcher thisbuf)
      text
      (extern_callback outbuf sender forum)
      handle_cmd
      argv


let discard_command iobuf cmd = ()

let handle_error iobuf str =
  prerr_endline ("!!! " ^ str)

let main () =
  let handler = ref "/bin/true" in
  let inputfn = ref "" in
  let nick = ref "bot" in
  let user = ref "bot" in
  let mode = ref "+i" in
  let realname = ref "I'm a little printf, short and stdout" in
  let connect = ref [||] in
  let append_connect s = connect := Array.append !connect [|s|] in
  let speclist =
    [
      ("-n", Arg.Set_string nick, "Nickname");
      ("-u", Arg.Set_string user, "Username");
      ("-m", Arg.Set_string mode, "Mode");
      ("-r", Arg.Set_string realname, "Real name");
      ("-a", Arg.Set_string handler, "IRC message handler");
      ("-i", Arg.Set_string inputfn, "Command FIFO");
    ]
  in
  let usage = "usage: bot [OPTIONS] CONNECT-COMMAND [ARGS ...]" in
    Arg.parse speclist append_connect usage;
    if (Array.length !connect) < 1 then begin
      prerr_endline "Error: must specify connect command.";
      prerr_endline "";
      prerr_endline "Run with --help for usage information.";
      exit 64                           (* EX_USAGE *)
    end;

    let dispatcher = Dispatch.create () in
    let conn_out, conn_in = Process.spawn (!connect).(0) !connect in
    let iobuf_out = Iobuf.create dispatcher conn_out "out"
      discard_command
      handle_error
    in
    let _ = Iobuf.create dispatcher conn_in "in"
      (handle_command iobuf_out !handler)
      handle_error
    in
      write iobuf_out "NICK" [!nick] None;
      write iobuf_out "USER" [!user; !mode; "merf"] (Some !realname);
      Dispatch.run dispatcher

let _ =
  main ()
