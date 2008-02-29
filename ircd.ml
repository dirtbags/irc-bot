let dbg msg a =
  prerr_endline msg;
  a

(** Establish a server on the given address.

    [connection_handler] will be called with the file descriptor of
    any new connections.
*)
let establish_server ues connection_handler addr =
  let g = Unixqueue.new_group ues in
  let handle_event ues esys e =
    match e with
      | Unixqueue.Input_arrived (g, fd) ->
	  let cli_fd, cli_addr = Unix.accept fd in
	    connection_handler cli_fd
      | _ ->
	  raise Equeue.Reject
  in
  let srv = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind srv addr;
    Unix.listen srv 50;
    Unix.setsockopt srv Unix.SO_REUSEADDR true;
    Unixqueue.add_handler ues g handle_event;
    Unixqueue.add_resource ues g (Unixqueue.Wait_in srv, -.1.0)

let main () =
  let ues = Unixqueue.create_unix_event_system () in
  let g = Unixqueue.new_group ues in
  let handle_connection fd =
    ignore (Client.create ues g fd);
    Unixqueue.add_resource ues g (Unixqueue.Wait_in fd, -.1.0)
  in
    Unixqueue.add_handler ues g Client.handle_event;
    establish_server
      ues
      handle_connection
      (Unix.ADDR_INET (Unix.inet_addr_any, 7777));
    ues#run ()

let _ =
  main ()
