type t

val write : t -> Command.t -> unit
val handle_connection : Unixqueue.event_system -> Unixqueue.group -> Unix.file_descr -> unit
  
