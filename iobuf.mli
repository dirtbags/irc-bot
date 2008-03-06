type t

val write : t -> Command.t -> unit
val bind : Unixqueue.event_system -> Unixqueue.group -> Unix.file_descr -> (t -> Command.t -> unit) -> unit
val rebind: t -> (t -> Command.t -> unit) -> unit
val close: t -> unit
val add_event_handler : Unixqueue.event_system -> Unixqueue.group -> unit

