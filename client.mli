type t

val create : Unixqueue.event_system -> Unixqueue.group -> Unix.file_descr -> t
val lookup : string -> t
val write : t -> Command.t -> unit

val handle_event : Unixqueue.event_system -> Unixqueue.event Equeue.t -> Unixqueue.event -> unit
  
