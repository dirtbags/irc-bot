type bot = {
  store: Infobot.t;
}

let write iobuf command args text =
  let cmd = Command.create None command args text in
    print_endline ("--> " ^ (Command.as_string cmd));
    Iobuf.write iobuf cmd

let msg iobuf recip text =
  write iobuf "PRIVMSG" [recip] (Some text)

let calc_re = Str.regexp "^calc \\(.*\\)"
let calc iobuf forum text =
  if Str.string_match calc_re text 0 then
    msg iobuf forum (Str.matched_group 1 text)

let handle_privmsg bot iobuf sender forum text =
  calc iobuf forum text;
  match (Infobot.lookup bot.store text) with
    | Some reply ->
      msg iobuf forum reply
    | None ->
      ()
      

let handle_command bot iobuf cmd =
  print_endline ("<-- " ^ (Command.as_string cmd));
  match (Command.as_tuple cmd) with
    | (Some sender, "PRIVMSG", [target], Some text) ->
      let forum =
        if Irc.is_channel target then
          target
        else
          sender
      in
      handle_privmsg bot iobuf sender forum text
    | (_, "PING", _, text) ->
        write iobuf "PONG" [] text
    | (_, "001", _, _) ->
        write iobuf "JOIN" ["#bot"] None
    | (Some sender, "JOIN", [], Some chan) ->
        msg iobuf chan "hi asl"
    | _ ->
        ()

let handle_error iobuf str =
  print_endline str

let main () =
  let host = Unix.gethostbyname "woozle.org" in
  let dispatcher = Dispatch.create () in
  let conn = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let bot = {store = Infobot.create "info.cdb"} in
  let _ = Unix.connect conn (Unix.ADDR_INET (host.Unix.h_addr_list.(0), 6667)) in
  let iobuf = Iobuf.create dispatcher conn "woozle"
    (handle_command bot)
    handle_error
  in
    write iobuf "NICK" ["bot"] None;
    write iobuf "USER" ["bot"; "bot"; "bot"] (Some "A Bot");
    Dispatch.run dispatcher
    

let _ =
  main ()
