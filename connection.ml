open Unixqueue

exception Buffer_overrun

(** Generic equeue connection class. *)
class virtual connection
  (ues : unix_event_system)
  ?(input_timeout = -.1.0)
  fd =
object (self)
  val g = ues#new_group ()
  val mutable debug = false

  initializer
    ues#add_handler g self#handle_event;
    ues#add_resource g (Wait_in fd, input_timeout)

  method debug v =
    debug <- v

  method log msg =
    if debug then
      print_endline msg

  method handle_event ues esys e =
    match e with
      | Input_arrived (g, fd) ->
	  self#input_ready fd
      | Output_readiness (g, fd) ->
	  self#output_ready fd
      | Out_of_band (g, fd) ->
	  self#handle_oob fd
      | Timeout (g, op) ->
	  self#handle_timeout op
      | Signal ->
	  self#handle_signal ()
      | Extra exn ->
	  self#handle_extra exn

  method virtual output_ready : Unix.file_descr -> unit
    
  method virtual input_ready : Unix.file_descr -> unit

  method handle_oob fd =
    self#log "Unhandled OOB";
    raise Equeue.Reject

  method handle_timeout op =
    self#log "Unhandled timeout";
    raise Equeue.Reject

  method handle_signal () =
    self#log "Unhandled signal";
    raise Equeue.Reject

  method handle_extra exn =
    self#log "Unhandled extra";
    raise Equeue.Reject

  method handle_close () =
    self#log "Closed"

end


(** Bare connection for reading and writing.

    You can inherit this and define appropriate [handle_*] methods.
    A [write] method is provided for your convenience.

*)
class bare_connection
  (ues : unix_event_system)
  ?(input_timeout = -.1.0)
  ?(output_timeout = -.1.0)
  ?(input_max = 1024)
  ?(output_max = 1024)
  fd =
object (self)
  inherit connection ues ~input_timeout fd


  val obuf = String.create output_max
  val mutable obuf_len = 0

  method write data =
    let data_len = String.length data in
      if (data_len + obuf_len > output_max) then
	raise Buffer_overrun;
      String.blit data 0 obuf obuf_len data_len;
      obuf_len <- obuf_len + data_len;
      ues#add_resource g (Wait_out fd, output_timeout)

  method output_ready fd =
    let size = obuf_len in
    let n = Unix.single_write fd obuf 0 size in
      obuf_len <- obuf_len - n;
      if (obuf_len = 0) then
	(* Don't check for output readiness anymore *)
	begin
	  ues#remove_resource g (Wait_out fd)
	end
      else
	(* Put unwritten output back into the output queue *)
	begin
	  String.blit obuf n obuf 0 (obuf_len)
	end

  method input_ready fd =
    let data = String.create input_max in
    let len = Unix.read fd data 0 input_max in
      if (len > 0) then
	self#handle_input (String.sub data 0 len)
      else
	begin
	  self#handle_close ();
	  Unix.close fd;
	  ues#clear g;
	end

  method handle_input data =
    self#log ("<-- [" ^ (String.escaped data) ^ "]")


end


(** Write s to fd, returning any unwritten data. *)
let write fd s =
  let sl = String.length s in
  let n = Unix.single_write fd s 0 sl in
    (String.sub s n (sl - n))

(** Buffered connection class.

    Input is split by newlines and sent to [handle_line].

    Output is done with [write].  Send a list of words to be joined by a
    space.  This is intended to make one-to-many communications more
    memory-efficient: the common strings need not be copied to all
    recipients.
*)
class virtual buffered_connection
  (ues : unix_event_system) 
  ?(output_timeout = -.1.0)
  ?(ibuf_max = 4096)
  ?(max_outq = 50)
  ?(max_unsent = 4096)
  fd =
object (self)
  inherit connection ues fd

  (* This allocates a string of length ibuf_max for each connection.
     That could add up. *)
  val mutable ibuf = String.create ibuf_max
  val mutable ibuf_len = 0

  val mutable unsent = ""
  val mutable outq = Queue.create ()

  method output_ready fd =
    (* This could be better optimized, I'm sure. *)
    match (unsent, Queue.is_empty outq) with
      | ("", true) ->
	  ues#remove_resource g (Wait_out fd)
      | ("", false) ->
	  let s = (String.concat " " (Queue.pop outq)) ^ "\n" in
	    unsent <- write fd s;
	    if (unsent = "") then
	      self#output_ready fd
      | (s, _) ->
	  unsent <- write fd s;
	  if (unsent = "") then
	    self#output_ready fd


  method virtual handle_line : string -> unit

  (** Split ibuf on newline, feeding each split into self#handle_input.
      
      Does not send the trailing newline.  You can add it back if you want.
  *)
  method split_handle_input () =
    match ibuf with
      | "" ->
	  ()
      | ibuf ->
	  let p = String.index ibuf '\n' in
	  let s = String.sub ibuf 0 p in
	    if p >= ibuf_len then
	      raise Not_found;
	    ibuf_len <- ibuf_len - (p + 1);
	    String.blit ibuf (p + 1) ibuf 0 ibuf_len;
	    self#handle_line s;
	    self#split_handle_input ()

  method input_ready fd =
    let size = ibuf_max - ibuf_len in
    let len = Unix.read fd ibuf ibuf_len size in
      if (len > 0) then
	begin
	  ibuf_len <- ibuf_len + len;
	  try
	    self#split_handle_input ()
	  with Not_found ->
	    if (ibuf_len = ibuf_max) then
	      (* No newline found, and the buffer is full *)
	      raise Buffer_overrun;
	end
      else
	begin
	  self#handle_close ();
	  Unix.close fd;
	  ues#clear g;
	end

  method write line =
    if (Queue.length outq) >= max_outq then
      raise (Failure "Maximum output queue length exceeded")
    else
      begin
	Queue.add line outq;
	ues#add_resource g (Wait_out fd, output_timeout)
      end
end


(** Establish a server on the given address.

    [connection_handler] will be called with the file descriptor of
    any new connections.
*)
let establish_server ues connection_handler addr =
  let g = ues#new_group () in
  let handle_event ues esys e =
    match e with
      | Input_arrived (g, fd) ->
	  let cli_fd, cli_addr = Unix.accept fd in
	    connection_handler cli_fd
      | _ ->
	  raise Equeue.Reject
  in
  let srv = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind srv addr;
    Unix.listen srv 50;
    Unix.setsockopt srv Unix.SO_REUSEADDR true;
    ues#add_handler g handle_event;
    ues#add_resource g (Wait_in srv, -.1.0)


