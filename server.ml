open Irc

(* ==========================================
 * Server stuff
 *)
let create () =
  {clients_by_name = Hashtbl.create 25;
   clients_by_file_descr = Hashtbl.create 25;
   channels_by_name = Hashtbl.create 10}

let get_client_by_name srv name =
  Hashtbl.find srv.clients_by_name name

let get_client_by_file_descr srv fd =
  Hashtbl.find srv.clients_by_file_descr fd

let get_channel_by_name srv name =
  Hashtbl.find srv.channels_by_name name
