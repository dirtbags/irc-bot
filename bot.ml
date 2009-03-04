let info_db = Cdb.open_cdb_in "/home/neale/src/firebot/info.cdb"
let _ = Random.self_init ()

let choice l =
  let n = Random.int (List.length l) in
    List.nth l n

let get_one key =
  let matches = Cdb.get_matches info_db key in
  match Stream.npeek 120 matches with
    | [] -> raise Not_found
    | keys -> choice keys

let write iobuf command args text =
  let cmd = Command.create None command args text in
    print_endline ("--> " ^ (Command.as_string cmd));
    Iobuf.write iobuf cmd

let scheme_eval str =
  let thread = Ocs_top.make_thread () in
  let env = Ocs_top.make_env () in
  let inport = Ocs_port.open_input_string str in
  let outport = Ocs_port.open_output_string () in
  let lexer = Ocs_lex.make_lexer inport "interactive" in
  let v = Ocs_read.read_expr lexer in
  let c = Ocs_compile.compile env v in
    try
      Ocs_eval.eval thread (Ocs_print.print outport false) c;
      Ocs_port.get_output_string outport
    with Ocs_error.Error msg ->
      msg


let handle_privmsg iobuf sender target text =
  try
    let factoid = get_one text in
    let response = 
      match factoid.[0] with
        | ':' ->
            "\001ACTION " ^ (Str.string_after factoid 1) ^ "\001"
        | '\\' ->
            Str.string_after factoid 1
        | _ ->
            Printf.sprintf "Gosh, %s, I think %s is %s" sender text factoid
    in
      write iobuf "PRIVMSG" [target] (Some response)
  with Not_found ->
    if text.[0] == '(' then
      let result = scheme_eval text in
        write iobuf "PRIVMSG" [target] (Some result)


let handle_command iobuf cmd =
  print_endline ("<-- " ^ (Command.as_string cmd));
  match Command.as_tuple cmd with
    | (_, "PING", _, text) ->
        write iobuf "PONG" [] text
    | (_, "001", _, _) ->
        write iobuf "JOIN" ["#bot"] None
    | (Some sender, "JOIN", [], Some chan) ->
        write iobuf "PRIVMSG" [chan] (Some "hi asl")
    | (Some sender, "PRIVMSG", [target], Some text) ->
        if Irc.is_channel target then
          handle_privmsg iobuf sender target text
    | _ ->
        ()

let handle_error iobuf str =
  print_endline str

let main () =
  let host = Unix.gethostbyname "woozle.org" in
  let dispatcher = Dispatch.create 5 in
  let conn = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let _ = Unix.connect conn (Unix.ADDR_INET (host.Unix.h_addr_list.(0), 6667)) in
  let iobuf = Iobuf.create dispatcher conn "woozle" handle_command handle_error in
    write iobuf "NICK" ["bot"] None;
    write iobuf "USER" ["bot"; "bot"; "bot"] (Some "A Bot");
    Dispatch.run dispatcher
    

let _ =
  main ()
