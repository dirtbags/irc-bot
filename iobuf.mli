type t

type command_handler = t -> Command.t -> unit
type error_handler = t -> string -> unit

val create : Dispatch.t -> Unix.file_descr -> Unix.sockaddr -> command_handler -> error_handler -> unit
val close: t -> string -> unit

val addr : t -> string
val write : t -> Command.t -> unit
val bind : t -> command_handler -> error_handler -> unit
