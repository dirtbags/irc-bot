open Unixqueue

exception Buffer_overrun

(** Generic equeue connection class.

    Input is line-buffered: handle_data is only called once a complete
    line has been read.  If the line is longer than the size of the
    input queue, you get an Input_buffer_overrun exception.

    You can inherit this and define appropriate [handle_*] methods.
    A [write] method is provided for your convenience.
    *)
class connection
  (ues : unix_event_system)
  ?(input_max = 1024)
  ?(output_max = 1024)
  fd =
object (self)

  val g = ues#new_group ()
  val mutable debug = false

  val obuf = String.create output_max
  val mutable obuf_len = 0
  val ibuf = String.create input_max
  val mutable ibuf_len = 0

  initializer
    ues#add_handler g self#handle_event;
    ues#add_resource g (Wait_in fd, -.1.0)

  method debug v =
    debug <- v

  method log msg =
    if debug then
      prerr_endline msg

  method write data =
    let data_len = String.length data in
      if (data_len + obuf_len > output_max) then
	raise Buffer_overrun;
      String.blit data 0 obuf obuf_len data_len;
      obuf_len <- obuf_len + data_len;
      ues#add_resource g (Wait_out fd, -.1.0)


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
	    self#handle_input s;
	    self#split_handle_input ()

  method input_ready fd =
    let size = input_max - ibuf_len in
    let len = Unix.read fd ibuf ibuf_len size in
      if (len > 0) then
	begin
	  ibuf_len <- ibuf_len + len;
	  try
	    self#split_handle_input ()
	  with Not_found ->
	    if (ibuf_len = output_max) then
	      (* No newline found, and the buffer is full *)
	      raise Buffer_overrun;
	end
      else
	begin
	  self#handle_close ();
	  Unix.close fd;
	  ues#clear g;
	end
      

      

  method handle_input data =
    self#log ("<-- [" ^ (String.escaped data) ^ "]");

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
    self#log "Closed";
    ()

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


