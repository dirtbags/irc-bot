module String_map = 
  Map.Make (struct
              type t = string
              let compare = compare
            end)

type client = Iobuf.t * Irc.nuhost

type t = {name: string;
          modes: string ref;
          clients: client String_map.t ref}

let modes = "aimnqpsrtklb"

let by_name = ref String_map.empty

let is_channel_name name =
  match name.[0] with
    | '#' | '&' | '!' | '+' ->
        true
    | _ ->
        false

let has_mode chan mode =
  String.contains !(chan.modes) mode

(* Channels handle:
   
  NICK, MODE, JOIN, PART, QUIT, TOPIC, NAMES, LIST, INVITE, KICK, PRIVMSG, NOTICE
*)

let write iobuf command args text =
  Iobuf.write iobuf (Command.create (Some !(Irc.name)) command args text)

let broadcast ?(metoo=false) chan sender command args text =
  let sender_iobuf, sender_nuhost = sender in
  let cmd = 
    Command.create
      (Some (Irc.string_of_nuhost sender_nuhost))
      command
      args
      text
  in
  let bwrite _ (iobuf, nuhost) =
    if (metoo || (nuhost <> sender_nuhost)) then
      Iobuf.write iobuf cmd
  in
    String_map.iter bwrite !(chan.clients)

let reply iobuf nick num ?(args=[]) text =
  write iobuf num (nick :: args) (Some text)

let handle_action (cli_iobuf, cli_nuhost) chan_name action args text =
  let chanopt = try
    Some (String_map.find chan_name !by_name)
  with Not_found ->
    None
  in
  let nick = Irc.nick cli_nuhost in
    match (action, chanopt, args, text) with
      | ("NOTICE", Some chan, [], Some text)
      | ("PRIVMSG", Some chan, [], Some text) ->
          if String_map.mem nick !(chan.clients) then
            broadcast chan (cli_iobuf, cli_nuhost) action [chan_name] (Some text)
          else
            reply cli_iobuf nick "404" ~args:[chan_name] "Cannot send to channel (join first)"
      | ("JOIN", _, _, None) ->
          let chan =
            match chanopt with
              | Some chan -> 
                  chan
              | None ->
                  let c = {name = chan_name; modes = ref ""; clients = ref String_map.empty} in  
                    by_name := String_map.add chan_name c !by_name;
                    c
          in
            if String_map.mem nick !(chan.clients) then
              (* Apparently we're expected to drop the command *)
              ()
            else
              let me = (cli_iobuf, cli_nuhost) in
                chan.clients := String_map.add nick me !(chan.clients);
                broadcast ~metoo:true chan me "JOIN" [chan.name] None
      | (_, None, _, _) ->
          reply cli_iobuf nick "403" ~args:[chan_name] "No such channel"
      | _ -> 
          ()

