type t

val create : ?sender:string option -> ?text:string option -> string -> string list -> t
val from_string : string -> t
val as_string : t -> string
val as_tuple : t -> (string option * string * string list * string option)

val sender : t -> string option
val name : t -> string
val args : t -> string list
val text : t -> string option
