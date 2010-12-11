type t

type command_handler = t -> Command.t -> unit
type error_handler = t -> string -> unit

val create : Dispatch.t -> Unix.file_descr -> string -> command_handler -> error_handler -> t
val close: t -> string -> unit

val name : t -> string
val dispatcher : t -> Dispatch.t
val write : t -> Command.t -> unit
val bind : t -> command_handler -> error_handler -> unit
