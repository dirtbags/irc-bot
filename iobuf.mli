type t

val addr : t -> string

val write : t -> Command.t -> unit
val bind : Unixqueue.event_system -> Unixqueue.group -> Unix.file_descr -> (t -> Command.t -> unit) -> (unit -> unit) -> unit
val rebind: t -> (t -> Command.t -> unit) -> (unit -> unit) -> unit
val close: t -> unit
val handle_event : Unixqueue.event_system -> Unixqueue.event Equeue.t -> Unixqueue.event -> unit
