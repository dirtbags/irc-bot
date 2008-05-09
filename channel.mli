type t

val modes : string

val handle_command : Iobuf.t -> Irc.nuhost -> Command.t -> unit
val is_channel_name : string -> bool

