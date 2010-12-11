type bot = {
  store: Infobot.t;
}

let debug = prerr_endline

let file_descr_of_int (i:int) =
  let blob = Marshal.to_string i [] in
  (Marshal.from_string blob 0 : Unix.file_descr)

let write iobuf command args text =
  let cmd = Command.create None command args text in
    debug ("--> " ^ (Command.as_string cmd));
    Iobuf.write iobuf cmd

let msg iobuf recip text =
  write iobuf "PRIVMSG" [recip] (Some text)

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
        if line.[0] == ':' then
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

let handle_privmsg bot iobuf sender forum text =
  if text.[0] == '.' then
    Process.create_canned
      (Iobuf.dispatcher iobuf)
      text
      (extern_callback iobuf sender forum)
      "./helper"
      [|"./helper"; sender; forum|]
  else
    Infobot.handle_privmsg bot.store (msg iobuf forum) sender forum text

let handle_command bot outbuf thisbuf cmd =
  debug ("<-- " ^ (Command.as_string cmd));
  match (Command.as_tuple cmd) with
    | (Some suhost, "PRIVMSG", [target], Some text) ->
        let sender = Irc.nick (Irc.nuhost_of_string suhost) in
        let forum =
          if Irc.is_channel target then
            target
          else
            sender
        in
          handle_privmsg bot outbuf sender forum text
    | (_, "PING", _, text) ->
        write outbuf "PONG" [] text
    | (_, "001", _, _) ->
        write outbuf "JOIN" ["#bot"] None;
    | (Some sender, "JOIN", [], Some chan) ->
        msg outbuf chan "hi asl"
    | _ ->
        ()

let discard_command iobuf cmd = ()

let handle_error iobuf str =
  prerr_endline ("!!! " ^ str)

let main () =
  let bot = {store = Infobot.create "info.cdb"} in
  let dispatcher = Dispatch.create () in

  let iobuf_out = Iobuf.create dispatcher Unix.stdout "collab_out"
    discard_command
    handle_error
  in
  let _ = Iobuf.create dispatcher Unix.stdin "collab_in"
    (handle_command bot iobuf_out)
    handle_error
  in
    write iobuf_out "NICK" ["zinc"] None;
    write iobuf_out "USER" ["zinc"; "zinc"; "zinc"] (Some "I'm a little printf, short and stdout");
    Dispatch.run dispatcher
    

let _ =
  main ()
