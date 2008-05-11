type t

val modes : string

(** [handle_action (cli_iobuf, cli_nuhost) chan_name action args text]
    handles [action] on [chan_name] with arguments [args] and text
    [text], sent by [cli_nuhost] from [cli_iobuf] *)
val handle_action : (Iobuf.t * Irc.nuhost) -> string -> string -> string list -> string option -> unit
val is_channel_name : string -> bool

