(** (Nickname, username, hostname) tuple *)
type nuhost = (string * string * string)

val name : string ref
val version : string
val start_time : float

val is_channel : string -> bool
val uppercase : string -> string
val lowercase : string -> string
val truncate : string -> int -> string

val nuhost_of_string : string -> nuhost
val string_of_nuhost : nuhost -> string
val nick : nuhost -> string
val user : nuhost -> string
val host : nuhost -> string
