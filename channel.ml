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

let handle_command cli nuhost cmd =
  match (Command.as_tuple cmd) with
    | (None, "JOIN", ["0"], None) ->
        (* Leave all channels *)
        failwith "XXX: JOIN 0"
    | (None, "JOIN", [name], None) ->
        let nick = Irc.nick nuhost in
        let chan = 
          try
            String_map.find name !by_name
          with Not_found ->
            let c = {name = name; modes = ref ""; clients = ref String_map.empty} in  
              by_name := String_map.add name c !by_name;
              c
        in
          if String_map.mem nick !(chan.clients) then
            (* Apparently we're expected to drop the command *)
            ()
          else
            let me = (cli, nuhost) in
              chan.clients := String_map.add nick me !(chan.clients);
              broadcast ~metoo:true chan me "JOIN" [name] None
    | _ -> 
        ()
