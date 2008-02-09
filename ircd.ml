open Unixqueue

class ircd_connection (ues : unix_event_system) fd =
object (self)
  inherit Connection.line_connection ues fd

  method handle_line line =
    let parts = Pcre.split ~pat:" " line in
      match parts with
	| ["NICK"; nick] ->
	    self#log ("Set nickname to " ^ nick);
	    self#write ":testserver.test NOTICE nick :*** Hi there.\n";
	    self#write "PING :12345\n";
	| _ ->
	    self#log ("Unknown: " ^ line)
      
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

