(* Buffered I/O, Scheme ports.  *)

type port

val input_port : in_channel -> port
val output_port : out_channel -> port
val open_input_port : string -> port
val open_output_port : string -> port
val open_input_string : string -> port
val open_output_string : unit -> port
val get_output_string : port -> string

val is_input : port -> bool
val is_output : port -> bool

val getc : port -> char option
val ungetc : port -> char -> unit
val char_ready : port -> bool

val putc : port -> char -> unit
val puts : port -> string -> unit

val flush : port -> unit

val close : port -> unit

