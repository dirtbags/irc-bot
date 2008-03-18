type t

val write_command : t -> Command.t -> unit
val write : t -> string option -> string -> string list -> string option -> unit
val handle_connection : Dispatch.t -> Unix.file_descr -> Unix.sockaddr -> unit
  
