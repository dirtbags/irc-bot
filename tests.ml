open Unixqueue
open OUnit
open Chat
open Irc

let do_chat script () =
  let ircd_instance ues fd =
    let g = Unixqueue.new_group ues in
      Iobuf.add_event_handler ues g;
      Iobuf.bind ues g fd (Client.create_command_handler ())
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
		    [Send "WELCOME :datacomp\r\n";
		     Recv "WELCOME :datacomp\r\n"]));
    ]

let _ =
  Irc.name := "testserver.test";
  run_test_tt_main (TestList [unit_tests; regression_tests])
	
  
