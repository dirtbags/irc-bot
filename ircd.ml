let dbg msg a =
  prerr_endline msg;
  a

(** Establish a server on the given address.

    [connection_handler] will be called with the file descriptor of
    any new connections.
*)
let establish_server d connection_handler addr =
  let rec handle_event fd events =
    match events with
      | [] ->
	  ()
      | Dispatch.Input :: tl ->
	  let cli_fd, cli_addr = Unix.accept fd in
	    connection_handler cli_fd cli_addr;
	    handle_event fd tl
      | Dispatch.Hangup :: tl ->
	  Dispatch.delete d fd;
	  handle_event fd tl
      | _ :: tl ->
	  handle_event fd tl
  in
  let srv = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind srv addr;
    Unix.listen srv 50;
    Unix.setsockopt srv Unix.SO_REUSEADDR true;
    Dispatch.add d fd handle_event [Dispatch.Input]

let main () =
  let d = Dispatch.create 50 in
    establish_server
      ues
      (Client.handle_connection d)
      (Unix.ADDR_INET (Unix.inet_addr_any, 6667));
    Dispatch.run d

let _ =
  main ()
