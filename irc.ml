open Uq_engines
open Unixqueue

let newline_re = Pcre.regexp "\n\r?"
let argsep_re = Pcre.regexp " :"
let space_re = Pcre.regexp " "

let string_map f s =
  let l = String.length s in
  if l = 0 then
    s
  else
    let r = String.create l in
      for i = 0 to l - 1 do
	String.unsafe_set r i (f (String.unsafe_get s i))
      done;
      r

let lowercase_char c =
  if (c >= 'A' && c <= '^') then
    Char.unsafe_chr(Char.code c + 32)
  else
    c

let uppercase_char c =
  if (c >= 'a' && c <= '~') then
    Char.unsafe_chr(Char.code c - 32)
  else
    c

let uppercase s = string_map uppercase_char s
let lowercase s = string_map lowercase_char s

    
class irc (ues : unix_event_system) =
object (self)

  val mutable debug = false
  val throttle_interval = 1.0

  (** Group for this bot's events *)
  val g = ues#new_group ()

  val out_wait_id = ues#new_wait_id ()
  val mutable check_output = None
  val mutable output_pending = ""
  val mutable input_pending = ""
  val outq_immediate = Queue.create ()
  val outq_throttled = Queue.create ()
  val outq_last_sent = Unix.time ()

  initializer
    ues#add_handler g self#handle_event

  method debug v =
    debug <- v

  method log msg =
    if debug then
      prerr_endline msg

  method handle_event ues' esys e =
    assert (ues' = ues);
    match e with
      | Input_arrived (g, fd) ->
	  self#handle_input fd
      | Output_readiness (g, fd) ->
	  self#handle_output fd
      | Out_of_band (g, fd) ->
	  self#handle_oob fd
      | Timeout (g, op) ->
	  self#handle_timeout op
      | Signal ->
	  self#handle_signal ()
      | Extra exn ->
	  self#handle_extra exn

  method handle_input fd =
    let s = 4096 in
    let buf = String.create s in
    let len = Unix.read fd buf 0 s in
    let input = input_pending ^ (String.sub buf 0 len) in
      if (input <> "") then
	let lines = Pcre.split ~rex:newline_re input in
	let rec handle_lines lines =
	  match lines with
	    | [] 
	    | [""] ->
		input_pending <- "";
	    | line :: tl ->
		self#handle_line line;
		handle_lines tl
	in
	  handle_lines lines
      else
	begin
	  Unix.close fd;
	  ues#clear g;
	end
      

  method handle_output fd =
    let data = (
      if (output_pending <> "") then 
	output_pending
      else if not (Queue.is_empty outq_immediate) then
	Queue.pop outq_immediate
      else if not (Queue.is_empty outq_throttled) then
	begin
	  (* Stop listening for output events; add a timeout to
	     start listening again *)
	  (match check_output with
	     | None -> ()
	     | Some co -> 
		 let pay_attention () =
		   ues#add_resource g (co, -.1.0)
		 in
		 ues#remove_resource g co;
		 ues#once g throttle_interval pay_attention
	  );
	  Queue.pop outq_throttled
	end
      else
	match check_output with
	  | None -> ""
	  | Some co ->
	      ues#remove_resource g co;
	      ""
    ) in
    let data_len = String.length data in
    let n = Unix.single_write fd data 0 data_len in
      output_pending <- String.sub data n (data_len - n);
      if (data <> "") then
	self#log ("--> " ^ (String.escaped data))

  method handle_oob fd =
    self#log "OOB ready!";
    raise Equeue.Reject

  method handle_timeout op =
    self#log "Timeout!";
    raise Equeue.Reject

  method handle_signal () =
    self#log "Signal!";
    raise Equeue.Reject

  method handle_extra exn =
    self#log "Extra!";
    raise Equeue.Reject

  method handle_line line =
    let argstr, txt =
      match Pcre.split ~max:2 ~rex:argsep_re line with
	| [] -> ("", "")
	| [a] -> (a, "")
	| [a; b] -> (a, b)
	| _ -> ("", "")
    in
    let sender, args =
      let args' = Pcre.split ~rex:space_re argstr in
	if (debug) then
	  print_endline ("<-- [" ^
			   (String.concat "; " args') ^
			   "] " ^
			   txt);
	if (List.hd args').[0] = ':' then
	  (List.hd args', List.tl args')
	else
	  ("", args')
    in
      match args with
	| [] ->
	    ()
	| "NOTICE" :: args ->
	    let tlen = String.length txt in
	      if ((txt.[0] = '\001') &&
		    (txt.[tlen - 1] = '\001')) then
		self#handle_ctcp_reply sender args (String.sub txt 1 (tlen - 2))
	      else
		self#handle_notice sender args txt
	| "PRIVMSG" :: args ->
	    let tlen = String.length txt in
	      if ((txt.[0] = '\001') &&
		    (txt.[tlen - 1] = '\001')) then
		self#handle_ctcp_request sender args (String.sub txt 1 (tlen - 2))
	      else
		self#handle_privmsg args sender txt
	| ["PING"] ->
	    self#handle_ping txt
	| str :: args ->
	    let numeric =
	      try
		Some (int_of_string str)
	      with Failure _ ->
		None
	    in
	      match numeric with
		| Some n ->
		    self#handle_numeric n sender args txt
		| None ->
		    self#handle_unknown sender args txt
	

  method handle_privmsg sender args txt =
    ()

  method handle_notice sender args txt =
    ()

  method handle_ctcp_request sender args txt =
    ()

  method handle_ctcp_reply sender args txt =
    ()

  method handle_numeric n sender args txt =
    ()

  method handle_unknown sender args txt =
    self#log ("Got unknown server message")

  method handle_ping txt =
    self#send ["PONG"] txt

  (** Public methods *)

  method set_fd fd nick gecos =
    (* XXX: Clear old junk *)
    check_output <- Some (Wait_out fd);
    ues#add_resource g (Wait_in fd, -.1.0);
    self#send ~now:true ["USER"; nick; "+iw"; nick] gecos;
    self#send ~now:true ["NICK"; nick] ""

  (** Send a command to the IRC server *)
  method send ?(now=false) args txt =
    match check_output with
      | None -> ()
      | Some co ->
	  let q = (if now then outq_immediate else outq_throttled) in
	  let cmdstr =
	    (String.concat " " args) ^
	      (if txt = "" then "" else " :") ^
	      txt ^
	      "\n"
	  in
	    Queue.push cmdstr q;
	    ues#add_resource g (co, -.1.0)

  (** Send a private message *)
  method privmsg ?(now=false) recipient txt =
    self#send ~now ["PRIVMSG"; recipient] txt

  (** Send a notice *)
  method notice ?(now=false) recipient txt =
    self#send ~now ["NOTICE"; recipient] txt

  (** Send a CTCP request *)
  method ctcp_request ?(now=false) recipient command txt =
    self#privmsg ~now recipient ("\001" ^ command ^ " " ^ txt ^ "\001")

  (** Send a CTCP reply *)
  method ctcp_reply ?(now=false) recipient command txt =
    self#notice ~now recipient ("\001" ^ command ^ " " ^ txt ^ "\001")

end


let main() =
  let ues = new unix_event_system () in
  let c = connector (`Socket(`Sock_inet_byname(Unix.SOCK_STREAM,
                                               "woozle.org", 6667),
                             default_connect_options
                            )) ues in
    when_state
      ~is_done:(fun connstat ->
		  match connstat with
		    | `Socket(fd, _) ->
			let b = new irc ues in
			  b#set_fd fd "plasma" "Plasma Bot"
		    | _ -> assert false
	       )
      c;
    Unixqueue.run ues
