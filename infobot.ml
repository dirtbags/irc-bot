let info_db = Cdb.open_cdb_in "/home/neale/src/firebot/info.cdb"
let _ = Random.self_init ()

let choice l =
  let n = Random.int (List.length l) in
    List.nth l n

let choose_one key =
  let matches = Cdb.get_matches info_db key in
  match Stream.npeek 120 matches with
    | [] -> raise Not_found
    | keys -> choice keys

let handle_privmsg iobuf sender target text =
  try
    let factoid = choose_one text in
    let response = 
      match factoid.[0] with
        | ':' ->
            "\001ACTION " ^ (Str.string_after factoid 1) ^ "\001"
        | '\\' ->
            Str.string_after factoid 1
        | _ ->
            Printf.sprintf "I've heard that %s is %s" text factoid
    in
      Iobuf.write iobuf (Command.create None "PRIVMSG" [target] (Some response))
  with Not_found ->
    ()

let handle_command iobuf cmd =
  print_endline ("<I- " ^ (Command.as_string cmd));
  match Command.as_tuple cmd with
    | (Some sender, "PRIVMSG", [target], Some text) ->
        if Irc.is_channel target then
          handle_privmsg iobuf sender target text
    | _ ->
        ()

let _ = Plugin.register handle_command
let _ = print_endline "========= INFOBOT"
