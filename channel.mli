type t

(* Channels handle:
   
  MODE, JOIN, PART, TOPIC, NAMES, LIST, INVITE, KICK, PRIVMSG, NOTICE
*)

val modes : string

val handle_command : Iobuf.t -> Irc.nuhost -> Command.t -> unit
val is_channel_name : string -> bool

