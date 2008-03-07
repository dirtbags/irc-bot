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
          addr: string;
          command_handler: (t -> Command.t -> unit) ref;
	  close_handler: (unit -> unit) ref}

let ibuf_max = 4096
let max_outq = 50
let obuf_max = 4096

let by_file_descr = Hashtbl.create 25

let addr iobuf = iobuf.addr

let write iobuf cmd =
  let was_empty = Queue.is_empty iobuf.outq in
    Queue.add cmd iobuf.outq;
    if (was_empty && (!(iobuf.unsent) = ""))  then
      Unixqueue.add_resource
        iobuf.ues iobuf.grp (Unixqueue.Wait_out iobuf.fd, -.1.0)

let close iobuf =
  !(iobuf.close_handler) ();
  Hashtbl.remove by_file_descr iobuf.fd;
  Unix.close iobuf.fd;
  Unixqueue.remove_resource iobuf.ues iobuf.grp (Unixqueue.Wait_in iobuf.fd);
  try
    Unixqueue.remove_resource iobuf.ues iobuf.grp (Unixqueue.Wait_out iobuf.fd);
  with Not_found ->
    ()

let handle_close fd =
  try
    let iobuf = Hashtbl.find by_file_descr fd in
      close iobuf
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
	    !(iobuf.command_handler) iobuf parsed;
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

let bind ues grp fd command_handler close_handler =
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
  let iobuf = {ues = ues;
               grp = grp;
               fd = fd;
               outq = outq;
               unsent = unsent;
               ibuf = ibuf;
               ibuf_len = ibuf_len;
               addr = addr;
               command_handler = ref command_handler;
	       close_handler = ref close_handler}
  in
    Hashtbl.replace by_file_descr fd iobuf;
    Unixqueue.add_resource ues grp (Unixqueue.Wait_in fd, -.1.0);
    Unixqueue.add_close_action ues grp (fd, handle_close)

let rebind t command_handler close_handler =
  t.command_handler := command_handler;
  t.close_handler := close_handler
