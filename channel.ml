module String_map = 
  Map.Make (struct
              type t = string
              let compare = compare
            end)

type client = Iobuf.t * Irc.nuhost

type t = {name: string;
          modes: string ref;
          clients: client String_map.t}

let modes = "aimnqpsrtklb"

let channels = String_map.empty

let is_channel_name name =
  match name.[0] with
    | '#' | '&' | '!' | '+' ->
        true
    | _ ->
        false

let has_mode chan mode =
  String.contains !(chan.modes) mode

let handle_command iobuf (nick, username, hostname) cmd =
  match (Command.as_tuple cmd) with
    | _ -> 
        ()
