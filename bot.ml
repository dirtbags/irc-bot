let write iobuf command args text =
  let cmd = Command.create None command args text in
    print_endline ("--> " ^ (Command.as_string cmd));
    Iobuf.write iobuf cmd

let handle_command iobuf cmd =
  print_endline ("<-- " ^ (Command.as_string cmd));
  match Command.as_tuple cmd with
    | (_, "PING", _, text) ->
        write iobuf "PONG" [] text
    | (_, "001", _, _) ->
        write iobuf "JOIN" ["#bot"] None
    | (Some sender, "JOIN", [], Some chan) ->
        write iobuf "PRIVMSG" [chan] (Some "hi asl")
    | _ ->
        ()

let handle_error iobuf str =
  print_endline str

let main () =
  let host = Unix.gethostbyname "woozle.org" in
  let dispatcher = Dispatch.create 5 in
  let conn = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let _ = Unix.connect conn (Unix.ADDR_INET (host.Unix.h_addr_list.(0), 6667)) in
  let iobuf = Iobuf.create dispatcher conn "woozle" Plugin.handle_command handle_error in
    Plugin.register handle_command;
    write iobuf "NICK" ["bot"] None;
    write iobuf "USER" ["bot"; "bot"; "bot"] (Some "A Bot");
    Dispatch.run dispatcher
    

let _ =
  main ()
