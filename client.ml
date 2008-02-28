open Irc

(* ==========================================
 * Client stuff
 *)
let ibuf_max = 4096
let max_outq = 50
let obuf_max = 4096

let shutdown ues g fd =
  Unix.close fd;
  Unixqueue.remove_resource ues g (Unixqueue.Wait_in fd);
  try
    Unixqueue.remove_resource ues g (Unixqueue.Wait_out fd);
  with Not_found ->
    ()

let write cli line =
  let was_empty = Queue.is_empty cli.outq in
    Queue.add line cli.outq;
    if was_empty then
      cli.output_ready ()

let handle_close srv cli =
  ()

let handle_command_login srv cli command =
  (* Handle a command during the login phase *)
  match command.command with
    | "USER"
    | "NICK" ->
	()
    | _ ->
	print_endline "NO CAN DO SIR"

let rec handle_input srv cli =
    match cli.ibuf with
      | "" ->
	  ()
      | ibuf ->
	  let p = String.index ibuf '\n' in
	  let s = String.sub ibuf 0 p in
	    if p >= !(cli.ibuf_len) then
	      raise Not_found;
	    cli.ibuf_len := !(cli.ibuf_len) - (p + 1);
	    String.blit ibuf (p + 1) ibuf 0 !(cli.ibuf_len);
	    let parsed = Irc.command_of_string s in
	      cli.handle_command srv cli parsed;
	      handle_input srv cli

let create_event_handler srv =
  fun ues esys e ->
    match e with
      | Unixqueue.Input_arrived (g, fd) ->
	  let cli = Server.get_client_by_file_descr srv fd in
	  let size = ibuf_max - !(cli.ibuf_len) in
	  let len = Unix.read fd cli.ibuf !(cli.ibuf_len) size in
	    if (len > 0) then
	      begin
		cli.ibuf_len := !(cli.ibuf_len) + len;
		try
		  handle_input srv cli
		with Not_found ->
		  if (!(cli.ibuf_len) = ibuf_max) then
		    (* No newline found, and the buffer is full *)
		    raise (Failure "Buffer overrun");
	      end
	    else
	      shutdown ues g fd
      | Unixqueue.Output_readiness (g, fd) ->
	  print_endline "out"
      | Unixqueue.Out_of_band (g, fd) ->
	  print_endline "oob"
      | Unixqueue.Timeout (g, op) ->
	  print_endline "timeout"
      | Unixqueue.Signal ->
	  print_endline "signal"
      | Unixqueue.Extra exn ->
	  print_endline "extra"


let create ues g fd =
  {outq = Queue.create ();
   unsent = ref "";
   ibuf = String.create ibuf_max;
   ibuf_len = ref 0;
   output_ready = 
      begin
	fun () -> Unixqueue.add_resource ues g (Unixqueue.Wait_out fd, -.1.0)
      end;
   handle_command = handle_command_login;
   channels = []}

