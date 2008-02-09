open Unixqueue

type chat_event =
  | Send of string
  | Recv of string

exception Chat_match of (chat_event * chat_event)
exception Chat_timeout of chat_event

let string_of_chat_event e =
  match e with
    | Send str ->
	("Send(\"" ^ (String.escaped str) ^ "\")")
    | Recv str ->
	("Recv(\"" ^ (String.escaped str) ^ "\")")

(** Return true if str starts with substr *)
let startswith str substr =
  let l = String.length substr in
    if l > String.length str then
      false
    else
      String.sub str 0 l = substr


(** Return all but the first index chars in a string *)
let string_after str index =
  let l = String.length str in
    String.sub str index (l - index)


(** Read a chunk of bytes from fd *)
let read_fd fd =
  let s = 4096 in
  let buf = String.create s in
  let len = Unix.read fd buf 0 s in
    String.sub buf 0 len
    

class chat_handler chatscript (ues : unix_event_system) fd =
object (self)
  inherit Connection.connection ues fd

  val mutable script = chatscript
  val inbuf = Buffer.create 4096

  initializer
    self#run_script ();
    self#pulse (Send "") ()


  method pulse hd () =
    if (List.hd script = hd) then
      raise (Chat_timeout hd)
    else
      ues#once g 2.0 (self#pulse (List.hd script))


  method run_script () =
    match script with
      | [] ->
	  Unix.close fd;
	  ues#clear g
      | Send buf :: tl ->
	  self#write buf;
	  script <- tl;
	  self#run_script ()
      | Recv buf :: tl ->
	  let buf_len = String.length buf in
	  let inbuf_str = Buffer.contents inbuf in
	    if (Buffer.length inbuf >= buf_len) then
	      if startswith inbuf_str buf then
		begin
		  script <- tl;
		  Buffer.clear inbuf;
		  Buffer.add_substring
		    inbuf
		    inbuf_str
		    buf_len
		    ((String.length inbuf_str) - buf_len);
		  self#run_script ()
		end
	      else
		raise (Chat_match (Recv inbuf_str,
				   Recv buf))
	    else
	      ()


  method handle_input data =
    Buffer.add_string inbuf data;
    self#run_script ()

end


(** Run a chat script

    [chat script proc] will create a Unix domain socket pair, call [proc
    ues fd] with the event system and one of the sockets, and then run
    [script] through it.
*)

let chat script proc =
  let ues = new unix_event_system () in
  let a,b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let _ = proc ues a in
  let _ = new chat_handler script ues b in
    try
      Unixqueue.run ues
    with
      | Chat_match (got, expected) ->
	  raise (Failure ("Chat_match; got " ^
			    (string_of_chat_event got) ^
			    ", expected " ^
			    (string_of_chat_event expected)))
      | Chat_timeout evt ->
	  raise (Failure ("Chat_timeout waiting for " ^
			    (string_of_chat_event evt)))

