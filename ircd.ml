type server = {clients_by_name: (string, client) Hashtbl.t;
	       clients_by_file_descr: (Unix.file_descr, client) Hashtbl.t;
	       channels_by_name: (string, channel) Hashtbl.t}
and client = {outq: string list Queue.t;
	      unsent: string ref;
	      ibuf: string;
	      ibuf_len: int ref;
	      out_ready: unit -> unit;
	      channels: channel list}
and channel = {name: string}

let dump msg a =
  prerr_endline msg;
  a

(* ==========================================
 * Server stuff
 *)
let create_server () =
  {clients_by_name = Hashtbl.create 25;
   clients_by_file_descr = Hashtbl.create 25;
   channels_by_name = Hashtbl.create 10}

let get_client_by_name srv name =
  Hashtbl.find srv.clients_by_name name

let get_client_by_file_descr srv fd =
  Hashtbl.find srv.clients_by_file_descr fd

let get_channel_by_name srv name =
  Hashtbl.find srv.channels_by_name name


(* ==========================================
 * Client stuff
 *)
let ibuf_max = 4096
let max_outq = 50
let obuf_max = 4096

let create_client ues g fd =
  {outq = Queue.create ();
   unsent = ref "";
   ibuf = String.create ibuf_max;
   ibuf_len = ref 0;
   out_ready = 
      begin
	fun () -> Unixqueue.add_resource ues g (Unixqueue.Wait_out fd, -.1.0)
      end;
   channels = []}

let client_shutdown ues g fd =
  Unix.close fd;
  Unixqueue.remove_resource ues g (Unixqueue.Wait_in fd);
  try
    Unixqueue.remove_resource ues g (Unixqueue.Wait_out fd);
  with Not_found ->
    ()

let client_handle_line srv cli line =
  print_endline line

let client_handle_close srv cli =
  ()

let rec client_handle_input srv cli =
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
	    client_handle_line srv cli s;
	    client_handle_input srv cli

let create_event_handler srv =
  fun ues esys e ->
    match e with
      | Unixqueue.Input_arrived (g, fd) ->
	  let cli = dump "input" get_client_by_file_descr srv fd in
	  let size = dump "size" ibuf_max - !(cli.ibuf_len) in
	  let len = dump "read" Unix.read fd cli.ibuf !(cli.ibuf_len) size in
	    if (len > 0) then
	      begin
		cli.ibuf_len := !(cli.ibuf_len) + len;
		try
		  client_handle_input srv cli
		with Not_found ->
		  if (!(cli.ibuf_len) = ibuf_max) then
		    (* No newline found, and the buffer is full *)
		    raise (Failure "Buffer overrun");
	      end
	    else
	      client_shutdown ues g fd
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


let main () =
  let srv = create_server () in
  let handle_event = create_event_handler srv in
  let ues = Unixqueue.create_unix_event_system () in
  let g = Unixqueue.new_group ues in
  let handle_connection fd =
    let cli = create_client ues g fd in
      Hashtbl.replace srv.clients_by_file_descr fd cli;
      Unixqueue.add_resource ues g (Unixqueue.Wait_in fd, -.1.0);
  in
    Unixqueue.add_handler ues g handle_event;
    Connection.establish_server
      ues
      handle_connection
      (Unix.ADDR_INET (Unix.inet_addr_any, 7777));
    ues#run ()

