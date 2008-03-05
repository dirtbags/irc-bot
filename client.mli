type t

val write : t -> Command.t -> unit
val create_command_handler : unit -> Iobuf.t -> Command.t -> unit
  
