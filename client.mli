val create : Unixqueue.event_system -> Unixqueue.group -> Unix.file_descr -> Irc.client
val create_event_handler : Irc.server -> (Unixqueue.event_system -> Unixqueue.event Equeue.t -> Unixqueue.event -> unit)
val write : Irc.client -> string list -> unit

  
