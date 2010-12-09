type t = {
  filename: string;
  mutable db: Cdb.cdb_file;
}

let _ = Random.self_init ()

let create filename =
  {
    filename = filename;
    db = Cdb.open_cdb_in filename;
  }

let choice l =
  let n = Random.int (List.length l) in
    List.nth l n

let strip s =
  let rec lastchar n =
    match s.[n-1] with
      | '.'
      | '!'
      | '?'
      | ' ' ->
          lastchar (n - 1)
      | _ ->
          n
  in
  let len = lastchar (String.length s) in
    if (len = String.length s) then
      None
    else
      Some (String.sub s 0 len)

let choose_one ib key =
  match (Cdb.get_matches ib.db key) with
    | [] ->
        raise Not_found
    | keys ->
        choice keys

let handle_privmsg store iobuf sender target text =
  try
    let text, factoid =
      try
        (text, choose_one store text)
      with Not_found ->
        match (strip text) with
          | None ->
              raise Not_found
          | Some stext ->
              (stext, choose_one store stext)
    in
    let response = 
      match factoid.[0] with
        | ':' ->
            "\001ACTION " ^ (Str.string_after factoid 1) ^ "\001"
        | '\\' ->
            Str.string_after factoid 1
        | _ ->
            Printf.sprintf "I overheard that %s is %s" text factoid
    in
      Iobuf.write iobuf (Command.create None "PRIVMSG" [target] (Some response))
  with Not_found ->
    ()

let handle_command store iobuf cmd =
  match Command.as_tuple cmd with
    | (Some sender, "PRIVMSG", [target], Some text) ->
        if Irc.is_channel target then
          handle_privmsg store iobuf sender target text
    | _ ->
        ()
