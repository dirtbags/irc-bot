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

let by_file_descr = Hashtbl.create 25
let by_nick = Hashtbl.create 25

let error num args text =
  Error (Command.create (Some !(Irc.name)) num args (Some text))

let close cli ues g fd =
  Hashtbl.remove by_nick !(cli.nick);
  Hashtbl.remove by_file_descr fd;
  Unix.close fd;
  Unixqueue.remove_resource ues g (Unixqueue.Wait_in fd);
  try
    Unixqueue.remove_resource ues g (Unixqueue.Wait_out fd);
  with Not_found ->
    ()

let write cli cmd =
  Iobuf.write cli.iobuf cmd

let reply cli num text =
  write cli (Command.create 
	       (Some !(Irc.name)) num [!(cli.nick)] (Some text))

let handle_command cli iobuf cmd =
  write cli cmd

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
