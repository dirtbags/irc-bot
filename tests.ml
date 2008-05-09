open OUnit

let dump x =
  Printf.ksprintf (fun str -> prerr_string str; flush stderr) x

(* Return true iff str starts with substr *)
let startswith str substr =
  let l = String.length substr in
    if l > String.length str then
      false
    else
      String.sub str 0 l = substr


(* ***************************************************
 * Epoll stuff
 * ***************************************************)

let int_of_file_descr fd = (Obj.magic fd) + 0

let rec epollevents_as_list events =
  match events with
    | [] ->
        []
    | Epoll.In :: tl ->
        "POLLIN" :: (epollevents_as_list tl)
    | Epoll.Priority :: tl ->
        "POLLPRI" :: (epollevents_as_list tl)
    | Epoll.Out :: tl ->
        "POLLOUT" :: (epollevents_as_list tl)
    | Epoll.Error :: tl ->
        "POLLERR" :: (epollevents_as_list tl)
    | Epoll.Hangup :: tl ->
        "POLLHUP" :: (epollevents_as_list tl)

let rec epollfds_as_list pfds =
  match pfds with
    | [] ->
        []
    | (fd, events) :: tl ->
        (Printf.sprintf "{fd=%d; events=%s}"
           (int_of_file_descr fd)
           (String.concat "|" (epollevents_as_list events))) :: 
          epollfds_as_list tl

let epollfds_as_string pfds =
  "[" ^ (String.concat ", " (epollfds_as_list pfds)) ^ "]"

let epollfd_as_string pfd =
  epollfds_as_string [pfd]

let epoll_expect e ?(n=3) l =
  let m = Epoll.wait e n 0 in
  assert_equal
    ~printer:epollfds_as_string
    (List.sort compare l)
    (List.sort compare m)


(* ***************************************************
 * Chat script stuff
 * ***************************************************)
type chat_event =
  | Send of string
  | Recv of string
  | Regex of string

exception Chat_match of (string * chat_event)
exception Chat_timeout of chat_event

let string_of_chat_event e =
  match e with
    | Send str ->
        ("Send (\"" ^ (String.escaped str) ^ "\")")
    | Recv str ->
        ("Recv (\"" ^ (String.escaped str) ^ "\")")
    | Regex str ->
        ("Regex (\"" ^ (String.escaped str) ^ "\")")

(* Return a [Dispatch.fd_handler] function to run script [s] *)
let chat d fd s =
  let script = ref s in
    (* Add some amount, dependent on fd, to the timeout value, so peers won't obliterate it *)
  let timer = (Unix.time ()) +. 1.0 +. (0.01 *. (float_of_int (int_of_file_descr fd))) in
  let obuf = Buffer.create 4096 in
  let ibuf = Buffer.create 4096 in
  let handle_timer _ =
    failwith (Printf.sprintf "fd=%d timeout waiting for %s" 
                (int_of_file_descr fd)
                (string_of_chat_event (List.hd !script)))
  in
  let nomatch got =
    failwith (Printf.sprintf "fd=%d\nexpecting %s\n             got %s"
                (int_of_file_descr fd)
                (string_of_chat_event (List.hd !script))
                (String.escaped got))
  in
  let rec run_script fd =
    match !script with
      | [] ->
          if ((Buffer.length obuf) = 0) then begin
            Dispatch.delete_timer d timer;
            (try
               Dispatch.delete d fd
             with (Failure _) ->
               ());
            Unix.close fd
          end
      | Send buf :: tl ->
          Buffer.add_string obuf buf;
          Dispatch.modify d fd [Dispatch.Input; Dispatch.Output];
          script := tl;
          run_script fd
      | Recv buf :: tl ->
          let buf_len = String.length buf in
          let ibuf_str = Buffer.contents ibuf in
            if ((Buffer.length ibuf) >= buf_len) then begin
              if startswith ibuf_str buf then begin
                script := tl;
                Buffer.clear ibuf;
                Buffer.add_substring
                  ibuf
                  ibuf_str
                  buf_len
                  ((String.length ibuf_str) - buf_len);
                run_script fd
              end else
                nomatch ibuf_str
            end else
              ()
      | Regex buf :: tl ->
          let ibuf_str = Buffer.contents ibuf in
          let matched = Str.string_match (Str.regexp buf) ibuf_str 0 in
            if (Buffer.length ibuf > 0) then
              if matched then
                let match_len = Str.match_end () in
                  script := tl;
                  Buffer.clear ibuf;
                  Buffer.add_substring
                    ibuf
                    ibuf_str
                    match_len
                    ((String.length ibuf_str) - match_len);
                  run_script fd
              else
                nomatch ibuf_str
            else
              ()

  in
  let rec handler fd events =
    match events with
      | [] ->
          ()
      | Dispatch.Input :: tl ->
          let s = String.create 4096 in
          let n = Unix.read fd s 0 4096 in
            Buffer.add_substring ibuf s 0 n;
            run_script fd;
            handler fd tl
      | Dispatch.Output :: tl ->
          begin
            if ((Buffer.length obuf) = 0) then
              Dispatch.modify d fd [Dispatch.Input]
            else
              let ostr = Buffer.contents obuf in
              let olen = Buffer.length obuf in
              let n = Unix.write fd ostr 0 olen in
                Buffer.clear obuf;
                Buffer.add_substring obuf ostr n (olen - n)
          end;
          handler fd tl
      | Dispatch.Hangup :: tl ->
          (* Stop listening to this fd, it will always return Hangup *)
          (try
             Dispatch.delete d fd
           with (Failure _) ->
             ())
      | _ ->
          failwith "Unexpected event"
  in
    Dispatch.add_timer d handle_timer timer;
    Dispatch.add d fd handler [Dispatch.Input];
    run_script fd
      

(* ***************************************************
 * The tests
 * ***************************************************)

let unit_tests =
  "Unit tests" >::: [
    "Epoll" >:: 
      (fun () -> 
         let a,b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
         let e = Epoll.create 1 in
         let expect = epoll_expect e in
           Epoll.ctl e Epoll.Add (a, [Epoll.Out; Epoll.In]);
           expect [(a, [Epoll.Out])];

           Epoll.ctl e Epoll.Modify (a, [Epoll.In; Epoll.Priority]);
           expect [];

           Epoll.ctl e Epoll.Add (b, [Epoll.Out; Epoll.In]);
           expect [(b, [Epoll.Out])];

           Epoll.ctl e Epoll.Modify (a, [Epoll.Out; Epoll.In]);
           expect [(a, [Epoll.Out]); (b, [Epoll.Out])];
           assert_equal
             1
             (List.length (Epoll.wait e 1 0));

           Epoll.ctl e Epoll.Modify (a, [Epoll.Out; Epoll.In]);
           expect [(a, [Epoll.Out]); (b, [Epoll.Out])];

           assert_equal
             2
             (Unix.write a "hi" 0 2);
           expect [(a, [Epoll.Out]); (b, [Epoll.In; Epoll.Out])];

           Epoll.ctl e Epoll.Delete (a, []);
           expect [(b, [Epoll.In; Epoll.Out])];
           assert_raises 
             (Failure "ocaml_epoll_ctl: No such file or directory")
             (fun () ->
                Epoll.ctl e Epoll.Modify (a, [Epoll.In; Epoll.Priority]));
           assert_raises 
             (Failure "ocaml_epoll_ctl: File exists")
             (fun () ->
                Epoll.ctl e Epoll.Add (b, [Epoll.In; Epoll.Priority]));
           expect [(b, [Epoll.In; Epoll.Out])];

           Unix.close a;
           expect [(b, [Epoll.In; Epoll.Out; Epoll.Hangup])];
           assert_raises 
             (Failure "ocaml_epoll_ctl: Bad file descriptor")
             (fun () ->
                Epoll.ctl e Epoll.Modify (a, [Epoll.In; Epoll.Priority]));

           Unix.close b;
           Epoll.destroy e
      );

    "Dispatch" >:: 
      (fun () ->
         let d = Dispatch.create 3 in
         let a,b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in

         let last_event = ref (a, []) in
         let rec handle fd events =
           last_event := (fd, events)
         in

         let last_timer = ref 0.0 in
         let handle_timer time =
           last_timer := time
         in

         let s = String.create 4096 in

           assert_equal 8 (Unix.write a "dispatch" 0 8);
           Dispatch.add d b handle [Dispatch.Input; Dispatch.Output];
           Dispatch.once d;
           assert_equal (b, [Dispatch.Input; Dispatch.Output]) !last_event;
           assert_equal 8 (Unix.read b s 0 4096);
           assert_equal "dispatch" (Str.string_before s 8);

           (let time = ((Unix.gettimeofday ()) +. 0.01) in
              Dispatch.add_timer d handle_timer time;
              Dispatch.add_timer d handle_timer ((Unix.gettimeofday ()) +. 10.0);

              assert_equal ~printer:string_of_float 0.0 !last_timer;
              Dispatch.once d;
              assert_equal ~printer:string_of_float 0.0 !last_timer;

              Dispatch.modify d b [Dispatch.Input];
              Dispatch.once d;
              if (!last_timer = 0.0) then
                (* Give it one chance *)
                Dispatch.once d;
              assert_equal ~printer:string_of_float time !last_timer;

              Dispatch.modify d b [Dispatch.Input; Dispatch.Output];
              assert_equal 6 (Unix.write a "gnarly" 0 6);
              Dispatch.once d;
              assert_equal (b, [Dispatch.Input; Dispatch.Output]) !last_event;
              assert_equal 6 (Unix.read b s 0 4096);

              assert_equal ~printer:string_of_float time !last_timer);

           Dispatch.once d;
           assert_equal (b, [Dispatch.Output]) !last_event;

           Dispatch.destroy d;
           Unix.close a;
           Unix.close b
      );

      "command_of_string" >:: 
        (fun () ->
           assert_equal
             ~printer:Command.as_string
             (Command.create None "NICK" ["name"] None)
             (Command.from_string "NICK name");
           assert_equal
             ~printer:Command.as_string
             (Command.create None "NICK" ["name"] None)
             (Command.from_string "nick name");
           assert_equal
             ~printer:Command.as_string
             (Command.create (Some "foo") "NICK" ["name"] None)
             (Command.from_string ":foo NICK name");
           assert_equal
             ~printer:Command.as_string
             (Command.create (Some "foo.bar") "PART" ["#foo"; "#bar"]
                (Some "ta ta"))
             (Command.from_string ":foo.bar PART #foo #bar :ta ta");
        );

      "Chat test" >::
        (fun () ->
           let d = Dispatch.create 3 in
           let a,b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
             chat d a
               [Send "banner";
                Recv "hi";
                Send "ehlo there, pleased to meet you"];
             chat d b
               [Recv "banner"; 
                Send "hi"; 
                Regex "ehlo .* you"];
             Dispatch.run d;
        );
    ]

let do_login nick =
  [
    Send ("USER " ^ nick ^ " +iw " ^ nick ^ " :gecos\r\n");
    Send ("NICK " ^ nick ^ "\r\n");
    Recv (":testserver.test 001 " ^ nick ^ " :Welcome to IRC.\r\n");
    Recv (":testserver.test 002 " ^ nick ^ " :I am testserver.test Running version " ^ Irc.version ^ "\r\n");
    Recv (":testserver.test 003 " ^ nick ^ " :This server was created " ^ (string_of_float Irc.start_time) ^ "\r\n");
    Recv (":testserver.test 004 " ^ nick ^ " :testserver.test 0.1 l aimnqpsrtklb\r\n");
  ]

let regression_tests =
  "Regression tests" >:::
    [
      "Simple connection" >::
        (fun () ->
           let script =
             (do_login "nick") @
               [
                 Send "BLARGH\r\n";
                 Recv ":testserver.test 421 nick BLARGH :Unknown or misconstructed command\r\n";
                 Send "MOTD\r\n";
                 Recv ":testserver.test 422 nick :MOTD File is missing\r\n";
                 Send "TIME\r\n";
                 Regex ":testserver\\.test 391 nick testserver\\.test :[-0-9]+T[:0-9]+Z\r\n";
                 Send "VERSION\r\n";
                 Recv ":testserver.test 351 nick 0.1 testserver.test :\r\n";
                 Send "PING snot\r\n";
                 Recv ":testserver.test PONG testserver.test :snot\r\n";
                 Send "PING :snot\r\n";
                 Recv ":testserver.test PONG testserver.test :snot\r\n";
                 Send "PONG snot\r\n";
                 Send "ISON nick otherguy\r\n";
                 Recv ":testserver.test 303 nick :nick\r\n";
                 Send "ISON otherguy thirdguy\r\n";
                 Recv ":testserver.test 303 nick :\r\n";
                 Send "PRIVMSG nick :hello\r\n";
                 Recv ":nick!nick@UDS PRIVMSG nick :hello\r\n";
                 Send "NOTICE nick :hello\r\n";
                 Recv ":nick!nick@UDS NOTICE nick :hello\r\n";
                 Send "PRIVMSG otherguy :hello\r\n";
                 Recv ":testserver.test 401 nick otherguy :No such nick/channel\r\n";
                 Send "AWAY :eating biscuits\r\n";
                 Recv ":testserver.test 306 nick :You have been marked as being away\r\n";
                 Send "AWAY\r\n";
                 Recv ":testserver.test 305 nick :You are no longer marked as being away\r\n";
                 Send "ERROR :I peed my pants\r\n";
                 Recv ":testserver.test NOTICE nick :Bummer.\r\n";
                 Send "INFO\r\n";
                 Recv (":testserver.test 371 nick :pgircd v" ^ Irc.version ^ "\r\n");
                 Recv (Printf.sprintf ":testserver.test 371 nick :Running since %f\r\n" Irc.start_time);
                 Recv ":testserver.test 374 nick :End of INFO list\r\n";
               ]
           in
           let d = Dispatch.create 2 in
           let a,b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
             Client.handle_connection d a (Unix.getpeername a);
             chat d b script;
             Dispatch.run d);

      "Second connection" >::
        (fun () ->
           let script =
             (do_login "otherguy") @
               [
                 Send "ISON nick otherguy\r\n";
                 Recv ":testserver.test 303 otherguy :otherguy\r\n";
               ]
           in
           let d = Dispatch.create 2 in
           let a,b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
             Client.handle_connection d a (Unix.getpeername a);
             chat d b script;
             Dispatch.run d);

      "Simultaneous connections" >::
        (fun () ->
           let script1 =
             (do_login "alice") @
               [
                 Send "ISON bob\r\n";
                 Recv ":testserver.test 303 alice :bob\r\n";
                 Send "JOIN #foo\r\n"; 
                 Recv ":alice!alice@UDS JOIN #foo\r\n";
                 Send "PRIVMSG bob :Hi Bob!\r\n";
                 Recv ":bob!bob@UDS JOIN #foo\r\n";
                 Send "QUIT :foo\r\n";
                 Recv ":testserver.test ERROR :So long\r\n";
               ]
           in
           let script2 =
             (do_login "bob") @
               [
                 Send "ISON alice\r\n";
                 Recv ":testserver.test 303 bob :alice\r\n";
                 Recv ":alice!alice@UDS PRIVMSG bob :Hi Bob!\r\n";
                 Send "JOIN #foo\r\n";
                 Recv ":bob!bob@UDS JOIN #foo\r\n";
                 Send "QUIT :foo\r\n";
                 Recv ":testserver.test ERROR :So long\r\n";
               ]
           in
           let d = Dispatch.create 4 in
           let aa,ab = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
           let ba,bb = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
             Client.handle_connection d aa (Unix.getpeername aa);
             Client.handle_connection d ba (Unix.getpeername ba);
             chat d ab script1;
             chat d bb script2;
             Dispatch.run d);
    ]

let _ =
  Irc.name := "testserver.test";
  run_test_tt_main (TestList [unit_tests; regression_tests])
