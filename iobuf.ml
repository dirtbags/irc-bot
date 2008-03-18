(* **************************************
 * IRC Command I/O buffers
 * **************************************)
type t = {d: Dispatch.t;
          fd: Unix.file_descr;
          outq: Command.t Queue.t;
          unsent: string ref;
          ibuf: string;
          ibuf_len: int ref;
          addr: Unix.sockaddr;
          handle_command: command_handler ref;
          handle_error: error_handler ref;
          valid: bool ref}
and command_handler = t -> Command.t -> unit
and error_handler = t -> string -> unit


let ibuf_max = 4096
let max_outq = 50
let obuf_max = 4096

let addr iobuf = 
  match iobuf.addr with
    | Unix.ADDR_UNIX s ->
        "UDS"
    | Unix.ADDR_INET (addr, port) ->
        Unix.string_of_inet_addr addr

let write iobuf cmd =
  let was_empty = Queue.is_empty iobuf.outq in
    Queue.add cmd iobuf.outq;
    if (was_empty && (!(iobuf.unsent) = ""))  then
      Dispatch.modify iobuf.d iobuf.fd [Dispatch.Input; Dispatch.Output]

let crlf = Str.regexp "\r?\n"

let handle_input iobuf =
  let buf = Str.string_before iobuf.ibuf !(iobuf.ibuf_len) in
  let lines = Str.split_delim crlf buf in
  let rec loop l =
    match l with
      | [] ->
	  ()
      | [leftover] ->
	  iobuf.ibuf_len := (String.length leftover);
	  String.blit leftover 0 iobuf.ibuf 0 !(iobuf.ibuf_len)
      | line :: tl ->
	  let parsed = Command.from_string line in
	    !(iobuf.handle_command) iobuf parsed;
	    loop tl
  in
    loop lines

let close iobuf message =
  !(iobuf.handle_error) iobuf message;
  iobuf.valid := false;
  Dispatch.modify iobuf.d iobuf.fd [Dispatch.Input; Dispatch.Output]

let rec handle_events iobuf fd events =
  match events with
    | [] ->
	()
    | Dispatch.Input :: tl ->
        let size = ibuf_max - !(iobuf.ibuf_len) in
        let len = Unix.read fd iobuf.ibuf !(iobuf.ibuf_len) size in
          iobuf.ibuf_len := !(iobuf.ibuf_len) + len;
          handle_input iobuf;
          if (!(iobuf.ibuf_len) = ibuf_max) then
            (* No newline found, and the buffer is full *)
	    close iobuf "Input buffer overrun"
	  else
	    handle_events iobuf fd tl
    | Dispatch.Output :: tl ->
        (* XXX: Could be optimized to try and fill the output buffer *)
        let buf =
          if (!(iobuf.unsent) = "") then
            let cmd = Queue.pop iobuf.outq in
              (Command.as_string cmd) ^ "\r\n"
          else
            !(iobuf.unsent)
        in
	let buflen = String.length buf in
        let n = Unix.single_write fd buf 0 buflen in
	  if n < buflen then begin
            iobuf.unsent := Str.string_after buf n;
	    handle_events iobuf fd tl
          end else if Queue.is_empty iobuf.outq then
            if !(iobuf.valid) then begin
	      Dispatch.modify iobuf.d fd [Dispatch.Input];
	      handle_events iobuf fd tl
            end else begin
              (* Close invalid connection after all output has despooled *)
              Dispatch.delete iobuf.d iobuf.fd;
              Unix.close iobuf.fd
            end
    | Dispatch.Priority :: tl ->
	let s = String.create 4096 in
	  ignore (Unix.recv fd s 0 4096 [Unix.MSG_OOB]);
	  handle_events iobuf fd tl
    | Dispatch.Error :: tl ->
	close iobuf "Error"
    | Dispatch.Hangup :: tl ->
	close iobuf "Hangup"

let bind iobuf handle_command handle_error =
  iobuf.handle_command := handle_command;
  iobuf.handle_error := handle_error

let create d fd addr handle_command handle_error =
  let iobuf = {d = d;
               fd = fd;
               outq = Queue.create ();
               unsent = ref "";
               ibuf = String.create ibuf_max;
               ibuf_len = ref 0;
               addr = addr;
               handle_command = ref handle_command;
               handle_error = ref handle_error;
               valid = ref true} in
    Dispatch.add d fd (handle_events iobuf) [Dispatch.Input]
