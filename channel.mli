type t

val modes : string

val lookup : string -> t
val create : string -> t
val is_channel_name : string -> bool

