type t

val write : t -> Command.t -> unit
val handle_connection : Dispatch.t -> Unix.file_descr -> Unix.sockaddr -> unit
  
