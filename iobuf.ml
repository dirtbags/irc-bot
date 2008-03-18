(* **************************************
 * IRC Command I/O buffers
 * **************************************)
type t = {d: Dispatch.t;
          fd: Unix.file_descr;
          outq: Command.t Queue.t;
          unsent: string ref;
          ibuf: string;
          ibuf_len: int ref;
          addr: string;
          command_handler: (t -> Command.t -> unit) ref;
	  close_handler: (string -> unit) ref}

let ibuf_max = 4096
let max_outq = 50
let obuf_max = 4096

let addr iobuf = iobuf.addr

let write iobuf cmd =
  let was_empty = Queue.is_empty iobuf.outq in
    Queue.add cmd iobuf.outq;
    if (was_empty && (!(iobuf.unsent) = ""))  then
      Dispatch.modify iobuf.d iobuf.fd [Dispatch.Input; Dispatch.Output]

let close iobuf message =
  !(iobuf.close_handler) message;
  Dispatch.delete iobuf.d iobuf.fd;
  Unix.close iobuf.fd

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
	    !(iobuf.command_handler) iobuf parsed;
	    loop tl
  in
    loop lines

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
	  if n < buflen then
            iobuf.unsent := Str.string_after buf n
	  else if Queue.is_empty iobuf.outq then
	    Dispatch.modify iobuf.d fd [Dispatch.Input];
	  handle_events iobuf fd tl
    | Dispatch.Priority :: tl ->
	let s = String.create 4096 in
	  ignore (Unix.recv fd s 0 4096 [Unix.MSG_OOB]);
	  handle_events iobuf fd tl
    | Dispatch.Error :: tl ->
	close iobuf "Error"
    | Dispatch.Hangup :: tl ->
	close iobuf "Hangup"


let bind d fd command_handler close_handler =
  let (outq, unsent, ibuf, ibuf_len) =
    (Queue.create (), ref "", String.create ibuf_max, ref 0)
  in
  let addr =
    match Unix.getpeername fd with
      | Unix.ADDR_UNIX s ->
          "UDS"
      | Unix.ADDR_INET (addr, port) ->
          Unix.string_of_inet_addr addr
  in
  let iobuf = {d = d;
               fd = fd;
               outq = outq;
               unsent = unsent;
               ibuf = ibuf;
               ibuf_len = ibuf_len;
               addr = addr;
               command_handler = ref command_handler;
	       close_handler = ref close_handler}
  in
    Dispatch.add d fd (handle_events iobuf) [Dispatch.Input]

let rebind t command_handler close_handler =
  t.command_handler := command_handler;
  t.close_handler := close_handler
