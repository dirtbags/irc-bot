open Unixqueue

class ircd_connection (ues : unix_event_system) fd =
object (self)
  inherit Connection.connection ues fd
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

let _ =
  main ()

