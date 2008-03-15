open Unixqueue
open OUnit
open Chat
open Irc

let ues = Unixqueue.create_unix_event_system ()

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

let epoll_expect e ?(n=3) l =
  let m = Epoll.wait e n 0 in
  assert_equal
    ~printer:epollfds_as_string
    (List.sort compare l)
    (List.sort compare m)

let unit_tests =
  "Unit tests" >::: [
    "epoll" >:: 
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
        )
    ]
      


let do_login nick =
  [
    Send ("USER " ^ nick ^ " +iw " ^ nick ^ " :gecos\r\n");
    Send ("NICK " ^ nick ^ "\r\n");
    Recv (":testserver.test 001 " ^ nick ^ " :Welcome to IRC.\r\n");
    Recv (":testserver.test 002 " ^ nick ^ " :I am testserver.test Running version " ^ Irc.version ^ "\r\n");
    Recv (":testserver.test 003 " ^ nick ^ " :This server was created sometime\r\n");
    Recv (":testserver.test 004 " ^ nick ^ " :testserver.test 0.1 l t\r\n");
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
               ]
           in
           let g = Unixqueue.new_group ues in
           let a,b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
             Unixqueue.add_handler ues g Iobuf.handle_event;
             Client.handle_connection ues g a;
             ignore (new chat_handler script ues b);
             chat_run ues);

      "Second connection" >::
        (fun () ->
           let script =
             (do_login "otherguy") @
               [
                 Send "ISON nick otherguy\r\n";
                 Recv ":testserver.test 303 otherguy :otherguy\r\n";
               ]
           in
           let g = Unixqueue.new_group ues in
           let a,b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
             Unixqueue.add_handler ues g Iobuf.handle_event;
             Client.handle_connection ues g a;
             ignore (new chat_handler script ues b);
             chat_run ues);

      "Simultaneous connections" >::
        (fun () ->
           let script1 =
             (do_login "alice") @
               [
                 Send "ISON bob\r\n";
                 Recv ":testserver.test 303 alice :bob\r\n";
                 Send "PRIVMSG bob :Hi Bob!\r\n";
                 Send "PING :foo\r\n";  (* Make sure we don't disconnect too soon *)
                 Recv ":testserver.test PONG testserver.test :foo\r\n";
               ]
           in
           let script2 =
             (do_login "bob") @
               [
                 Send "ISON alice\r\n";
                 Recv ":testserver.test 303 bob :alice\r\n";
                 Recv ":alice!alice@UDS PRIVMSG bob :Hi Bob!\r\n";
               ]
           in
           let g = Unixqueue.new_group ues in
           let aa,ab = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
           let ba,bb = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
             Unixqueue.add_handler ues g Iobuf.handle_event;
             Client.handle_connection ues g aa;
             Client.handle_connection ues g ba;
             ignore (new chat_handler script1 ues ab);
             ignore (new chat_handler script2 ues bb);
             chat_run ues);
    ]

let _ =
  Irc.name := "testserver.test";
  run_test_tt_main (TestList [Dispatch_tests.unit; unit_tests; regression_tests])
	
  
