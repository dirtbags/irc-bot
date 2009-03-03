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

let rec string_of_sval = function
  | Ocs_types.Snull -> "()"
  | Ocs_types.Seof -> "#<eof>"
  | Ocs_types.Strue -> "#t"
  | Ocs_types.Sfalse -> "#f"
  | Ocs_types.Sstring s -> s
  | Ocs_types.Ssymbol s -> s
  | Ocs_types.Sint i -> (string_of_int i)
  | Ocs_types.Sreal r -> (Ocs_numstr.string_of_real r)
  | Ocs_types.Scomplex z -> (Ocs_numstr.string_of_complex z)
  | Ocs_types.Sbigint b -> (Big_int.string_of_big_int b)
  | Ocs_types.Srational r -> (Ratio.string_of_ratio r)
  | Ocs_types.Schar c -> String.make 1 c
  | Ocs_types.Spair l -> "#<it's a pair>"
  | Ocs_types.Svector v -> "#<it's a vector>"
  | Ocs_types.Sport _ -> "#<port>"
  | Ocs_types.Sproc _ -> "#<procedure>"
  | Ocs_types.Sprim { Ocs_types.prim_fun = _; Ocs_types.prim_name = n } ->
      "#<primitive:" ^ n ^ ">"
  | Ocs_types.Spromise _ -> "#<promise>"
  | Ocs_types.Sesym (_, s) -> string_of_sval s
  | Ocs_types.Swrapped _ -> "#<wrapped>"
  | Ocs_types.Sunspec -> "#<unspecified>"
  | _ -> "#<unknown>"

let scheme_eval str =
  let thread = Ocs_top.make_thread () in
  let env = Ocs_top.make_env () in
  let inport = Ocs_port.string_input_port str in
  let lexer = Ocs_lex.make_lexer inport "interactive" in
  let v = Ocs_read.read_expr lexer in
  let c = Ocs_compile.compile env v in
  let buf = Buffer.create 20 in
  let printer v =
    Buffer.add_string buf (string_of_sval v)
  in
    try
      Ocs_eval.eval thread printer c;
      Buffer.contents buf
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
        if target.[0] = '#' then
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
    write iobuf "USER" ["bot"; "bot"; "bot"] (Some "Da Bot");
    Dispatch.run dispatcher
    

let _ =
  main ()
