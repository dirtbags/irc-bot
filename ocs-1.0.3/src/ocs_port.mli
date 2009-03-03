(* Buffered I/O, Scheme ports.  *)

type port

val input_port : in_channel -> port
val output_port : out_channel -> port
val open_input_port : string -> port
val open_output_port : string -> port
val string_input_port : string -> port
val string_output_port : unit -> port

val is_input : port -> bool
val is_output : port -> bool

val getc : port -> char option
val ungetc : port -> char -> unit
val char_ready : port -> bool

val putc : port -> char -> unit
val puts : port -> string -> unit

val flush : port -> unit

val close : port -> unit

