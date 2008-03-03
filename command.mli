type t

val create : string option -> string -> string list -> string option -> t
val from_string : string -> t
val as_string : t -> string
val as_tuple : t -> (string option * string * string list * string option)

val sender : t -> string option
val name : t -> string
val args : t -> string list
val text : t -> string option
