open Irc

(* ==========================================
 * Client stuff
 *)
type t = {outq: Command.t Queue.t;
          unsent: string ref;
          ibuf: string;
          ibuf_len: int ref;
          output_ready: unit -> unit;
          handle_command: t -> Command.t -> unit;
          nick: string ref;
          username: string ref;
          realname: string ref}


let by_file_descr = Hashtbl.create 25
let by_nick = Hashtbl.create 25

let ibuf_max = 4096
let max_outq = 50
let obuf_max = 4096

let lookup nick =
  Hashtbl.find by_nick nick

let lookup_fd fd =
  Hashtbl.find by_file_descr fd

let close cli ues g fd =
  Hashtbl.remove by_nick !(cli.nick);
  Hashtbl.remove by_file_descr fd;
  Unix.close fd;
  Unixqueue.remove_resource ues g (Unixqueue.Wait_in fd);
  try
    Unixqueue.remove_resource ues g (Unixqueue.Wait_out fd);
  with Not_found ->
    ()

let write cli cmd =
  let was_empty = Queue.is_empty cli.outq in
    Queue.add cmd cli.outq;
    if (was_empty && (!(cli.unsent) = ""))  then
      cli.output_ready ()

let handle_close cli =
  ()

let handle_command cli command =
  ()

let handle_command_login cli cmd =
  (* Handle a command during the login phase *)
  (match (Command.as_tuple cmd) with
     | (None, "USER", [username; _; _], Some realname) ->
         cli.username := username;
         cli.realname := Irc.truncate realname 40
     | (None, "NICK", [nick], None) ->
         cli.nick := nick;
     | _ ->
         write cli (Command.create 
                      ~sender:(Some !Irc.name)
                      ~text:(Some "Register first.")
                      "451" ["*"]));
  (match (!(cli.username), !(cli.nick)) with
     | ("", _)
     | (_, "") ->
	 ()
     | (_, nick) ->
	 write cli (Command.create
		      ~sender:(Some !Irc.name)
		      ~text:(Some "*** Hi there.")
		      "NOTICE"
		      [nick]))

let rec handle_input cli =
    match cli.ibuf with
      | "" ->
          ()
      | ibuf ->
          let p = 
	    let nlp = String.index ibuf '\n' in
	      if ((String.get ibuf (nlp - 1)) = '\r') then
		(nlp - 1)
	      else
		nlp
	  in
          let s = String.sub ibuf 0 p in
            if p >= !(cli.ibuf_len) then
              raise Not_found;
            cli.ibuf_len := !(cli.ibuf_len) - (p + 1);
            String.blit ibuf (p + 1) ibuf 0 !(cli.ibuf_len);
            let parsed = Command.from_string s in
              cli.handle_command cli parsed;
              handle_input cli

let handle_event ues esys e =
  match e with
    | Unixqueue.Input_arrived (g, fd) ->
        let cli = lookup_fd fd in
        let size = ibuf_max - !(cli.ibuf_len) in
        let len = Unix.read fd cli.ibuf !(cli.ibuf_len) size in
          if (len > 0) then
            begin
              cli.ibuf_len := !(cli.ibuf_len) + len;
              try
                handle_input cli
              with Not_found ->
                if (!(cli.ibuf_len) = ibuf_max) then
                  (* No newline found, and the buffer is full *)
                  raise (Failure "Buffer overrun");
            end
          else
            close cli ues g fd
    | Unixqueue.Output_readiness (g, fd) ->
        (* XXX: Could be optimized to try and fill the output buffer *)
        let cli = lookup_fd fd in
        let buf =
          if (!(cli.unsent) = "") then
            let cmd = Queue.pop cli.outq in
              (Command.as_string cmd) ^ "\r\n"
          else
            !(cli.unsent)
        in
	let buflen = String.length buf in
        let n = Unix.single_write fd buf 0 buflen in
	  if n < buflen then
            cli.unsent := Str.string_after buf n
	  else if Queue.is_empty cli.outq then
	    Unixqueue.remove_resource ues g (Unixqueue.Wait_out fd)
    | Unixqueue.Out_of_band (g, fd) ->
        print_endline "oob"
    | Unixqueue.Timeout (g, op) ->
        print_endline "timeout"
    | Unixqueue.Signal ->
        print_endline "signal"
    | Unixqueue.Extra exn ->
        print_endline "extra"


let create ues g fd =
  let cli =
    {outq = Queue.create ();
     unsent = ref "";
     ibuf = String.create ibuf_max;
     ibuf_len = ref 0;
     output_ready = 
	begin
          fun () -> Unixqueue.add_resource ues g (Unixqueue.Wait_out fd, -.1.0)
	end;
     handle_command = handle_command_login;
     nick = ref "";
     username = ref "";
     realname = ref ""}
  in
    Hashtbl.replace by_file_descr fd cli;
    cli

let set_nick cli nick =
  Hashtbl.remove by_nick !(cli.nick);
  Hashtbl.replace by_nick nick cli;
  cli.nick := nick
