open Unixqueue

class ircd_connection
  (ues : unix_event_system) 
  ?(output_timeout = -.1.0)
  ?(ibuf_max = 4096)
  ?(max_outq = 50)
  ?(max_unsent = 4096)
  fd =
object (self)
  inherit Connection.buffered_connection
    ues
    ~output_timeout
    ~ibuf_max
    ~max_outq
    ~max_unsent
    fd

  method handle_line line =
    let parts = Pcre.split ~pat:" " line in
      match parts with
	| ["NICK"; nick] ->
	    self#log ("Set nickname to " ^ nick);
	    self#write [":testserver.test"; "NOTICE"; nick; ":*** Hi there."];
	    self#write ["PING"; ":12345"];
	| _ ->
	    self#log ("Unknown: " ^ line)
      
  method die reason =
    self#log ("Dying: " ^ reason)
end

let main () =
  let ues = new unix_event_system () in
  let handle_connection fd =
    prerr_endline "hi!";
    let c = new ircd_connection ues fd in
      c#debug true
  in
    Connection.establish_server
       ues
      handle_connection
      (Unix.ADDR_INET (Unix.inet_addr_any, 7777));
    ues#run ()

