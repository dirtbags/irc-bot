open Irc

(* ==========================================
 * Client stuff
 *)
type t = {iobuf: Iobuf.t;
          nick: string ref;
          away: string option ref;
          username: string;
          realname: string}

exception Error of Command.t

let modes = "l"

let dbg msg a = prerr_endline msg; a

let by_nick = Hashtbl.create 25

let lookup nick =
  Hashtbl.find by_nick nick

let error num args text =
  Error (Command.create (Some !(Irc.name)) num args (Some text))

let nuhost cli = (!(cli.nick), cli.username, (Iobuf.addr cli.iobuf))

let kill cli message = 
  Iobuf.close cli.iobuf ("Killed: " ^ message)

let write_command cli cmd =
  Iobuf.write cli.iobuf cmd

let write cli sender name args text =
  write_command cli (Command.create sender name args text)

let reply cli num ?(args=[]) text =
  write cli (Some !(Irc.name)) num (!(cli.nick) :: args) (Some text)

let handle_error cli iobuf message =
  Hashtbl.remove by_nick !(cli.nick)

let handle_command cli iobuf cmd =
  match (Command.as_tuple cmd) with
    | (None, "OPER", [name; password], None) ->
        ()
    | (None, "MODE", target :: args, None) ->
        ()
    | (None, "SERVICE", [nickname; _; distribution; svctype; _], Some info) ->
        ()
    | (None, "QUIT", [], None) ->
        write cli (Some !(Irc.name)) "ERROR" [] (Some "So long");
        Iobuf.close iobuf "No reason provided"
    | (None, "QUIT", [], Some message) ->
        write cli (Some !(Irc.name)) "ERROR" [] (Some "So long");
        Iobuf.close iobuf message
    | (None, "JOIN", ["0"], None) ->
        ()
    | (None, "JOIN", [chan_name], None) ->
        Channel.handle_action (cli.iobuf, (nuhost cli)) chan_name "JOIN" [] None
    | (None, "JOIN", [channels; keys], None) ->
        ()
    | (None, "PART", [channels], message) ->
        ()
    | (None, "TOPIC", [channel], None) ->
        ()
    | (None, "TOPIC", [channel], Some topic) ->
        ()
    | (None, "NAMES", [channels], None) ->
        ()
    | (None, "LIST", [channels], None) ->
        ()
    | (None, "INVITE", [nickname; channel], None) ->
        ()
    | (None, "KICK", [channels; users], comment) ->
        ()
    | (None, ("PRIVMSG" as action), [target], Some text)
    | (None, ("NOTICE" as action), [target], Some text) ->
        if Channel.is_channel_name target then
          Channel.handle_action (cli.iobuf, (nuhost cli)) target action [] (Some text)
        else
          begin
            try
              let peer = lookup target in
                write peer 
                  (Some (Irc.string_of_nuhost (nuhost cli))) 
                  action [target] (Some text)
            with Not_found ->
              reply cli "401" ~args:[target] "No such nick/channel"
          end
    | (None, "MOTD", [], None) ->
        reply cli "422" "MOTD File is missing"
    | (None, "LUSERS", [], None) ->
        ()
    | (None, "VERSION", [], None) ->
        reply cli "351" ~args:[Irc.version; !(Irc.name)] ""
    | (None, "STATS", [], None) ->
        ()
    | (None, "TIME", [], None) ->
        let now = Unix.gmtime (Unix.time ()) in
        let timestr =
          Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
            (now.Unix.tm_year + 1900)
            now.Unix.tm_mday
            (match now.Unix.tm_mon with
               | 0 -> 12
               | mon -> mon)
            now.Unix.tm_hour
            now.Unix.tm_min
            now.Unix.tm_sec
        in
          reply cli "391" ~args:[!(Irc.name)] timestr;
    | (None, "ADMIN", [], None) ->
        ()
    | (None, "INFO", [], None) ->
        reply cli "371" (Printf.sprintf "pgircd v%s" Irc.version);
        reply cli "371" (Printf.sprintf "Running since %f" Irc.start_time);
        reply cli "374" "End of INFO list"
    | (None, "SERVLIST", [], None) ->
        ()
    | (None, "SQUERY", [servicename], Some text) ->
        ()
    | (None, "WHO", [], None) ->
        ()
    | (None, "WHO", [mask], None) ->
        ()
    | (None, "WHO", [mask; "o"], None) ->
        ()
    | (None, "WHIOS", [masks], None) ->
        ()
    | (None, "KILL", [nickname; comment], None) ->
        ()
    | (None, "PING", [], Some text)
    | (None, "PING", [text], None) ->
        write cli (Some !(Irc.name)) "PONG" [!(Irc.name)] (Some text)
    | (None, "PONG", [payload], None) ->
        (* We do nothing. *)
        ()
    | (None, "ERROR", [], Some message) ->
        write cli (Some !(Irc.name)) "NOTICE" [!(cli.nick)] (Some "Bummer.")
    | (None, "AWAY", [], None) ->
        cli.away := None;
        reply cli "305" "You are no longer marked as being away"
    | (None, "AWAY", [], Some message) ->
        cli.away := Some message;
        reply cli "306" "You have been marked as being away"
    | (None, "REHASH", [], None) ->
        ()
    | (None, "WALLOPS", [], Some text) ->
        ()
    | (None, "ISON", nicks, None) ->
	let ison = List.filter (Hashtbl.mem by_nick) nicks in
	  reply cli "303" (String.concat " " ison)
    | (_, name, _, _) ->
        reply cli "421" ~args:[name] "Unknown or misconstructed command"

let set_nick cli nick =
  if Hashtbl.mem by_nick nick then
    raise (error "433" [nick] "Nickname in use");
  Hashtbl.remove by_nick !(cli.nick);
  Hashtbl.replace by_nick nick cli;
  cli.nick := nick

let rec handle_command_prereg (nick, username, realname, password) iobuf cmd =
  (* Handle a command during the login phase *)
  let acc =
    match (Command.as_tuple cmd) with
      | (None, "PASS", [password'], None) ->
          (nick, username, realname, Some password')
      | (None, "USER", [username'; _; _], Some realname') ->
          (nick, Some username', Some (Irc.truncate realname' 40), password)
      | (None, "NICK", [nick'], None) ->
          (Some nick', username, realname, password)
      | _ ->
          Iobuf.write iobuf (Command.create
                               (Some !(Irc.name)) 
                               "451" ["*"] 
                               (Some "Register first."));
          (nick, username, realname, password)
  in
  let welcome cli =
    try
      set_nick cli !(cli.nick);
      reply cli "001" "Welcome to IRC.";
      reply cli "002" ("I am " ^ !(Irc.name) ^ 
                         " Running version " ^ Irc.version);
      reply cli "003" ("This server was created " ^ 
                         (string_of_float Irc.start_time));
      reply cli "004" (!(Irc.name) ^
                         " " ^ Irc.version ^
                         " " ^ modes ^
                         " " ^ Channel.modes);
      Iobuf.bind iobuf (handle_command cli) (handle_error cli)
    with Error cmd ->
      Iobuf.write iobuf cmd
  in
  match acc with
    | (Some nick, Some username, Some realname, None) ->
        welcome {iobuf = iobuf;
                 nick = ref nick;
                 away = ref None;
                 username = username;
                 realname = realname}
    | (Some nick, Some username, Some realname, Some password) ->
        Iobuf.write iobuf (Command.create
                             (Some !(Irc.name))
                             "NOTICE" ["AUTH"]
                             (Some "*** Authentication unimplemented"));
        welcome {iobuf = iobuf;
                 nick = ref nick;
                 away = ref None;
                 username = username;
                 realname = realname}
    | _ ->
        Iobuf.bind iobuf (handle_command_prereg acc) (fun _ _ -> ())

let handle_connection d fd addr =
  let handle_command = handle_command_prereg (None, None, None, None) in
    Iobuf.create d fd addr handle_command (fun _ _ -> ())
    
