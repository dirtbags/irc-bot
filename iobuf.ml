(* ==========================================
 * I/O buf stuff
 *)
type t = {ues: Unixqueue.event_system;
          grp: Unixqueue.group;
          fd: Unix.file_descr;
          outq: Command.t Queue.t;
          unsent: string ref;
          ibuf: string;
          ibuf_len: int ref;
          handle_command: (t -> Command.t -> unit) ref}

let ibuf_max = 4096
let max_outq = 50
let obuf_max = 4096

let by_file_descr = Hashtbl.create 25

let bind ues grp fd handle_command =
  let (outq, unsent, ibuf, ibuf_len) =
    (Queue.create (), ref "", String.create ibuf_max, ref 0)
  in
  let iobuf = {ues = ues;
               grp = grp;
               fd = fd;
               outq = outq;
               unsent = unsent;
               ibuf = ibuf;
               ibuf_len = ibuf_len;
               handle_command = ref handle_command}
  in
    Hashtbl.replace by_file_descr fd iobuf;
    Unixqueue.add_resource ues grp (Unixqueue.Wait_in fd, -.1.0)

let rebind t handle_command =
  t.handle_command := handle_command

let write iobuf cmd =
  let was_empty = Queue.is_empty iobuf.outq in
    Queue.add cmd iobuf.outq;
    if (was_empty && (!(iobuf.unsent) = ""))  then
      Unixqueue.add_resource
        iobuf.ues iobuf.grp (Unixqueue.Wait_out iobuf.fd, -.1.0)

let close iobuf =
  Hashtbl.remove by_file_descr iobuf.fd;
  Unix.close iobuf.fd;
  Unixqueue.remove_resource iobuf.ues iobuf.grp (Unixqueue.Wait_in iobuf.fd);
  try
    Unixqueue.remove_resource iobuf.ues iobuf.grp (Unixqueue.Wait_out iobuf.fd);
  with Not_found ->
    ()

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

let handle_event ues esys e =
  match e with
    | Unixqueue.Input_arrived (g, fd) ->
        let iobuf = Hashtbl.find by_file_descr fd in
        let size = ibuf_max - !(iobuf.ibuf_len) in
        let len = Unix.read fd iobuf.ibuf !(iobuf.ibuf_len) size in
          if (len > 0) then
            begin
              iobuf.ibuf_len := !(iobuf.ibuf_len) + len;
              try
                handle_input iobuf
              with Not_found ->
                if (!(iobuf.ibuf_len) = ibuf_max) then
                  (* No newline found, and the buffer is full *)
                  raise (Failure "Buffer overrun");
            end
          else
            close iobuf
    | Unixqueue.Output_readiness (g, fd) ->
        (* XXX: Could be optimized to try and fill the output buffer *)
        let iobuf = Hashtbl.find by_file_descr fd in
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
	    Unixqueue.remove_resource ues g (Unixqueue.Wait_out fd)
    | Unixqueue.Out_of_band (g, fd) ->
        print_endline "oob"
    | Unixqueue.Timeout (g, op) ->
        print_endline "timeout"
    | Unixqueue.Signal ->
        print_endline "signal"
    | Unixqueue.Extra exn ->
        print_endline "extra"

let add_event_handler ues g =
  Unixqueue.add_handler ues g handle_event
