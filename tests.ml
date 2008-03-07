open Unixqueue
open OUnit
open Chat
open Irc

let do_chat script () =
  let ircd_instance ues fd =
    let g = Unixqueue.new_group ues in
      Unixqueue.add_handler ues g Iobuf.handle_event;
      Client.handle_connection ues g fd
  in
    chat script ircd_instance

let unit_tests =
  "Unit tests" >:::
    [
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
	(do_chat ((do_login "nick") @
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
                    ]));

      "Second connection" >::
	(do_chat ((do_login "otherguy") @
		    [
		      Send "ISON nick otherguy\r\n";
		      Recv ":testserver.test 303 otherguy :otherguy\r\n";
		    ]));
    ]

let _ =
  Irc.name := "testserver.test";
  run_test_tt_main (TestList [unit_tests; regression_tests])
	
  
