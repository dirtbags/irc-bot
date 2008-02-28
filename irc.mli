type command = {sender: string option;
		command: string;
		args: string list;
		text: string option}

type server = {clients_by_name: (string, client) Hashtbl.t;
	       clients_by_file_descr: (Unix.file_descr, client) Hashtbl.t;
	       channels_by_name: (string, channel) Hashtbl.t}
and client = {outq: string list Queue.t;
	      unsent: string ref;
	      ibuf: string;
	      ibuf_len: int ref;
	      output_ready: unit -> unit;
	      handle_command: server -> client -> command -> unit;
	      channels: channel list}
and channel = {name: string}

val uppercase : string -> string
val lowercase : string -> string
val command_of_string : string -> command
val string_of_command : command -> string
