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

let lookup store text =
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
    match factoid.[0] with
      | ':' ->
        Some ("\001ACTION " ^ (Str.string_after factoid 1) ^ "\001")
      | '\\' ->
        Some (Str.string_after factoid 1)
      | _ ->
        Some (Printf.sprintf "I overheard that %s is %s" text factoid)
  with Not_found ->
    None


let handle_privmsg store msg sender forum text =
  match (lookup store text) with
    | Some reply ->
      msg reply
    | None ->
      ()
