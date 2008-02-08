open Unixqueue

type chat_event =
  | Send of string
  | Recv of string

exception Chat_match of (chat_event * chat_event)
exception Chat_failure of string

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
  val mutable script = chatscript
  val g = ues#new_group ()

  initializer
    ues#add_handler g self#handler;
    self#setup ()

  method setup () =
    match script with
      | [] ->
	  Unix.close fd;
	  ues#clear g
      | Send _ :: _ ->
	  ues#add_resource g (Wait_out fd, -.1.0);
	  begin
	    try
	      ues#remove_resource g (Wait_in fd)
	    with Not_found ->
	      ()
	  end
      | Recv _ :: _ ->
	  ues#add_resource g (Wait_in fd, -.1.0);
	  begin
	    try
	      ues#remove_resource g (Wait_out fd)
	    with Not_found ->
	      ()
	  end


  method handler ues' (esys : event Equeue.t) e =
    assert (ues = ues');
    match e with
      | Input_arrived (g, fd) ->
	  self#handle_input fd
      | Output_readiness (g, fd) ->
	  self#handle_output fd
      | _ ->
	  raise Equeue.Reject

  method handle_input fd =
    let buf = read_fd fd in
      match script with
	| Recv str :: tl ->
	    if (buf = str) then
	      begin
		script <- tl;
		self#setup()
	      end
	    else if startswith buf str then
	      begin
		script <- [Recv (string_after buf (String.length str))] @ tl;
		self#setup()
	      end
	    else
	      raise (Chat_match ((Recv str), (Recv buf)))
	| x :: tl ->
	    raise (Chat_match (x, (Recv buf)))
	| [] ->
	    raise (Chat_match ((Recv ""), (Recv buf)))


  method handle_output fd =
    match script with
      | Send str :: tl ->
	  let slen = String.length str in
	  let n = Unix.single_write fd str 0 slen in
	    if (n <> slen) then
	      script <- [Send (string_after str n)] @ tl
	    else
	      script <- tl;
	    self#setup()
      | x :: tl ->
	  raise (Chat_match (x, (Send "")))
      | [] ->
	  raise (Chat_match ((Recv ""), (Send "")))
  
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
    with Chat_match (got, expected) ->
      raise (Chat_failure ("Chat_match; got " ^
			     (string_of_chat_event got) ^
			     ", expected " ^
			     (string_of_chat_event expected)))

