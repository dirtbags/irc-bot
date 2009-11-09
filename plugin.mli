type handler = Iobuf.t -> Command.t -> unit

val register : handler -> unit
val unregister : handler -> unit
val handle_command : Iobuf.t -> Command.t -> unit
