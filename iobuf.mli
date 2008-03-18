type t

val addr : t -> string

val write : t -> Command.t -> unit
val bind : Dispatch.t -> Unix.file_descr -> (t -> Command.t -> unit) -> (string -> unit) -> unit
val rebind: t -> (t -> Command.t -> unit) -> (string -> unit) -> unit
val close: t -> string -> unit
