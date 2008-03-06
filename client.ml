open Irc



(* ==========================================
 * Client stuff
 *)
type t = {iobuf: Iobuf.t;
          nick: string ref;
          username: string;
          realname: string}

exception Error of Command.t

let modes = "l"

let dbg msg a = prerr_endline msg; a

let by_nick = Hashtbl.create 25

let error num args text =
  Error (Command.create (Some !(Irc.name)) num args (Some text))

let close cli ues g fd =
  Hashtbl.remove by_nick !(cli.nick);
  Unix.close fd;
  Unixqueue.remove_resource ues g (Unixqueue.Wait_in fd);
  try
    Unixqueue.remove_resource ues g (Unixqueue.Wait_out fd);
  with Not_found ->
    ()

let write cli cmd =
  Iobuf.write cli.iobuf cmd

let reply cli num ?(args=[]) text =
  write cli (Command.create 
               (Some !(Irc.name))
               num
               ([!(cli.nick)] @ args)
               (Some text))

let handle_command cli iobuf cmd =
  match (Command.as_tuple cmd) with
    | (None, "OPER", [name; password], None) ->
        ()
    | (None, "MODE", target :: args, None) ->
        ()
    | (None, "SERVICE", [nickname; _; distribution; svctype; _], Some info) ->
        ()
    | (None, "QUIT", [], message) ->
        ()
    | (None, "JOIN", ["0"], None) ->
        ()
    | (None, "JOIN", [channels], None) ->
        ()
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
    | (None, "PRIVMSG", [target], Some text) ->
        ()
    | (None, "NOTICE", [target], Some text) ->
        ()
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
        ()
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
        write cli (Command.create (Some !(Irc.name)) "PONG" [!(Irc.name)] (Some text))
    | (None, "PONG", [payload], None) ->
        ()
    | (None, "ERROR", [], Some message) ->
        ()
    | (None, "AWAY", [], None) ->
        ()
    | (None, "AWAY", [], Some message) ->
        ()
    | (None, "REHASH", [], None) ->
        ()
    | (None, "DIE", [], None) ->
        ()
    | (None, "RESTART", [], None) ->
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

let rec handle_command_prereg (nick', username', realname', password') iobuf cmd =
  (* Handle a command during the login phase *)
  let acc =
    match (Command.as_tuple cmd) with
      | (None, "PASS", [password], None) ->
          (nick', username', realname', Some password)
      | (None, "USER", [username; _; _], Some realname) ->
          (nick', Some username, Some (Irc.truncate realname 40), password')
      | (None, "NICK", [nick], None) ->
          (Some nick, username', realname', password')
      | _ ->
          Iobuf.write iobuf (Command.create
                               (Some !(Irc.name)) 
                               "451" ["*"] 
                               (Some "Register first."));
          (nick', username', realname', password')
  in
  let welcome cli =
    try
      set_nick cli !(cli.nick);
      reply cli "001" "Welcome to IRC.";
      reply cli "002" ("I am " ^ !(Irc.name) ^ 
                         " Running version " ^ Irc.version);
      reply cli "003" "This server was created sometime";
      reply cli "004" (!(Irc.name) ^
                         " " ^ Irc.version ^
                         " " ^ modes ^
                         " " ^ Channel.modes);
      Iobuf.rebind iobuf (handle_command cli)
    with Error cmd ->
      Iobuf.write iobuf cmd
  in
  match acc with
    | (Some nick, Some username, Some realname, None) ->
        welcome {iobuf = iobuf;
                 nick = ref nick;
                 username = username;
                 realname = realname}
    | (Some nick, Some username, Some realname, Some password) ->
        Iobuf.write iobuf (Command.create
                             (Some !(Irc.name))
                             "NOTICE" ["AUTH"]
                             (Some "*** Authentication unimplemented"));
        welcome {iobuf = iobuf;
                 nick = ref nick;
                 username = username;
                 realname = realname}
    | _ ->
        Iobuf.rebind iobuf (handle_command_prereg acc)

let create_command_handler () =
  handle_command_prereg (None, None, None, None)
