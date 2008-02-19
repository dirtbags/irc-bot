let ibuf_max = 4096
let max_outq = 50
let max_unsent = 4096

type client = {outq: Queue.t;
	       unsent: string ref;
	       ibuf: String;
	       ibuf_len: int ref;
	       out_ready: unit -> unit}


let new_client g fd =
  let cli = {outq = Queue.create ();
	     unsent = ref "";
	     ibuf = String.create ibuf_max;
	     ibuf_len = ref 0;
	     out_ready = fun () -> 
	       Unixqueue.add_resource g (Unixqueue.Wait_out fd, -.1.0)}
  in
    Unixqueue.add_resource g (Unixqueue.Wait_in fd, -.1.0);
    Unixqueue.add_handler g (handle_client_event cli);
    cli


let handle_client_event cli ues esys e =
  match e with
    | Input_arrived (g, fd) ->
	let size = ibuf_max - !cli.ibuf_len in
	let len = Unix.read fd cli.ibuf !cli.ibuf_len size in
	  if (len > 0) then
	    begin
	      cli.ibuf_len := !cli.ibuf_len + len;
	      try
		split_client_input cli ues
	      with Not_found ->
		if (!cli.ibuf_len = ibuf_max) then
		  (* No newline found, and the buffer is full *)
		  raise Buffer_overrun;
	    end
	  else
	    begin
	      
	      handle_close ();
	      Unix.close fd;
	      ues#clear g;
	    end
    | Output_readiness (g, fd) ->
	print_endline "Output ready"
    | Out_of_band (g, fd) ->
	print_endline "Amy is hotttt"
    | Timeout (g, op) ->
	print_endline "Timeout!!!1!!1 ZOMG!"
    | Signal ->
	print_endline "Signal"
    | Extra exn ->
	print_endline "Extra"


