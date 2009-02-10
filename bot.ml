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

let handle_command iobuf cmd =
  print_endline ("<-- " ^ (Command.as_string cmd));
  match Command.as_tuple cmd with
    | (_, "PING", _, text) ->
        write iobuf "PONG" [] text
    | (_, "001", _, _) ->
        write iobuf "JOIN" ["#bot"] None
    | (Some who, "JOIN", [], Some chan) ->
        write iobuf "PRIVMSG" [chan] (Some "hi asl")
    | (Some who, "PRIVMSG", [target], Some text) ->
        if target.[0] = '#' then
          try
            let factoid = get_one text in
            let response = 
              match factoid.[0] with
                | ':' ->
                    "\001ACTION " ^ (Str.string_after factoid 1) ^ "\001"
                | '\\' ->
                    Str.string_after factoid 1
                | _ ->
                    Printf.sprintf "Gosh, %s, I think %s is %s" who text factoid
            in
            write iobuf "PRIVMSG" [target] (Some response)
          with Not_found ->
            ()
        else
          ()
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
