(* **************************************
 * IRC Command I/O buffers
 * **************************************)
type t = {d: Dispatch.t;
          fd: Unix.file_descr;
          outq: Command.t Queue.t;
          unsent: string ref;
          ibuf: string;
          ibuf_len: int ref;
          name: string;
          handle_command: command_handler ref;
          handle_error: error_handler ref;
          alive: bool ref}
and command_handler = t -> Command.t -> unit
and error_handler = t -> string -> unit


let ibuf_max = 4096
let max_outq = 2000
let obuf_max = 4096

let name iobuf = iobuf.name

let dispatcher iobuf = iobuf.d

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
  iobuf.alive := false;
  Dispatch.modify iobuf.d iobuf.fd [Dispatch.Input; Dispatch.Output]

let write iobuf cmd =
  match Queue.length iobuf.outq with
    | a when a = max_outq ->
      close iobuf "Max outq exceeded"
    | len ->
      Queue.add cmd iobuf.outq;
      if ((len = 0) && (!(iobuf.unsent) = ""))  then
        Dispatch.modify iobuf.d iobuf.fd [Dispatch.Input; Dispatch.Output]

let handle_event iobuf fd event =
  match event with
    | Dispatch.Input ->
        let size = ibuf_max - !(iobuf.ibuf_len) in
          (match Unix.read fd iobuf.ibuf !(iobuf.ibuf_len) size with
             | 0 ->
                 close iobuf "Hangup"
             | len ->
                 iobuf.ibuf_len := !(iobuf.ibuf_len) + len;
                 handle_input iobuf;
                 if (!(iobuf.ibuf_len) = ibuf_max) then
                   (* No newline found, and the buffer is full *)
	           close iobuf "Input buffer overrun")
    | Dispatch.Output ->
        let buf = Buffer.create obuf_max in
          Buffer.add_string buf !(iobuf.unsent);
          while (((Buffer.length buf) < obuf_max) &&
                   (not (Queue.is_empty iobuf.outq))) do
            let cmd = Queue.pop iobuf.outq in
              Buffer.add_string buf (Command.as_string cmd);
              Buffer.add_string buf "\r\n"
          done;
          let bufstr = Buffer.contents buf in
	  let buflen = Buffer.length buf in
          let n = Unix.single_write fd bufstr 0 buflen in
	    if n < buflen then begin
              iobuf.unsent := Str.string_after bufstr n;
            end else if Queue.is_empty iobuf.outq then
              if !(iobuf.alive) then begin
                (* We're out of data to send *)
	        Dispatch.modify iobuf.d fd [Dispatch.Input];
              end else begin
                (* Close dead connection after all output has despooled *)
                Dispatch.delete iobuf.d iobuf.fd;
                Unix.close iobuf.fd
              end
    | Dispatch.Exception ->
	let s = String.create 4096 in
	  ignore (Unix.recv fd s 0 4096 [Unix.MSG_OOB])

let bind iobuf handle_command handle_error =
  iobuf.handle_command := handle_command;
  iobuf.handle_error := handle_error

let create d fd name handle_command handle_error =
  let iobuf = {d = d;
               fd = fd;
               outq = Queue.create ();
               unsent = ref "";
               ibuf = String.create ibuf_max;
               ibuf_len = ref 0;
               name = name;
               handle_command = ref handle_command;
               handle_error = ref handle_error;
               alive = ref true} in
    Dispatch.add d fd (handle_event iobuf) [Dispatch.Input];
    iobuf
