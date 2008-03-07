open Unixqueue

exception Buffer_overrun

type chat_event =
  | Send of string
  | Recv of string
  | Regex of string

exception Chat_match of (string * chat_event)
exception Chat_timeout of chat_event

let dbg msg a = prerr_endline msg; a
  
let string_of_chat_event e =
  match e with
    | Send str ->
        ("Send (\"" ^ (String.escaped str) ^ "\")")
    | Recv str ->
        ("Recv (\"" ^ (String.escaped str) ^ "\")")
    | Regex str ->
        ("Regex (\"" ^ (String.escaped str) ^ "\")")

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
    

class chat_handler chatscript 
  ?(input_timeout=0.1)
  ?(output_timeout = 0.1)
  ?(output_max = 4096)
  ?(input_max = 4096)
  (ues : unix_event_system) fd =
object (self)
  val g = ues#new_group ()
  val mutable debug = false


  val obuf = String.create output_max
  val mutable obuf_len = 0

  val mutable script = chatscript
  val inbuf = Buffer.create 4096

  initializer
    ues#add_handler g self#handle_event;
    ues#add_resource g (Wait_in fd, input_timeout);
    self#run_script ();

  method write data =
    let data_len = String.length data in
      if (data_len + obuf_len > output_max) then
        raise Buffer_overrun;
      String.blit data 0 obuf obuf_len data_len;
      obuf_len <- obuf_len + data_len;
      ues#add_resource g (Wait_out fd, output_timeout)

  method handle_event ues esys e =
    match e with
      | Input_arrived (g, fd) ->
          let data = String.create input_max in
          let len = Unix.read fd data 0 input_max in
            if (len > 0) then
              begin
                Buffer.add_string inbuf (String.sub data 0 len);
                self#run_script ()
              end
            else
              begin
                Unix.close fd;
                ues#clear g;
              end
      | Output_readiness (g, fd) ->
          let size = obuf_len in
          let n = Unix.single_write fd obuf 0 size in
            obuf_len <- obuf_len - n;
            if (obuf_len = 0) then
              (* Don't check for output readiness anymore *)
              begin
                ues#remove_resource g (Wait_out fd)
              end
            else
              (* Put unwritten output back into the output queue *)
              begin
                String.blit obuf n obuf 0 (obuf_len)
              end
      | Out_of_band (g, fd) ->
          raise (Failure "Out of band data")
      | Timeout (g, op) ->
          raise (Chat_timeout (List.hd script))
      | Signal ->
          raise (Failure "Signal")
      | Extra exn ->
          raise (Failure "Extra")

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
                raise (Chat_match (inbuf_str, Recv buf))
            else
              ()
      | Regex buf :: tl ->
          let inbuf_str = Buffer.contents inbuf in
          let matched = Str.string_match (Str.regexp buf) inbuf_str 0 in
            if (Buffer.length inbuf > 0) then
              if matched then
                let match_len = Str.match_end () in
                  script <- tl;
                  Buffer.clear inbuf;
                  Buffer.add_substring
                    inbuf
                    inbuf_str
                    match_len
                    ((String.length inbuf_str) - match_len);
                  self#run_script ()
              else
                raise (Chat_match (inbuf_str, Regex buf))
            else
              ()
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
      Unixqueue.run ues;
    with
      | Chat_match (got, expected) ->
          raise (Failure ("Not matched: got \"" ^
                            (String.escaped got) ^
                            "\"\n  expected " ^
                            (string_of_chat_event expected)))
      | Chat_timeout evt ->
          raise (Failure ("Timeout waiting for " ^
                            (string_of_chat_event evt)))

