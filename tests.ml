open Unixqueue
open OUnit
open Chat
open Irc

let do_chat script () =
  let ircd_instance ues fd =
    let g = Unixqueue.new_group ues in
      ignore (Client.create ues g fd);
      Unixqueue.add_handler ues g Client.handle_event;
      Unixqueue.add_resource ues g (Unixqueue.Wait_in fd, -.1.0)
  in
    chat script ircd_instance

let unit_tests =
  "Unit tests" >:::
    [
      "command_of_string" >:: 
	(fun () ->
	   assert_equal
	     ~printer:Command.as_string
	     (Command.create "NICK" ["name"])
	     (Command.from_string "NICK name");
	   assert_equal
	     ~printer:Command.as_string
	     (Command.create ~sender:(Some "foo") "NICK" ["name"])
	     (Command.from_string ":foo NICK name");
	   assert_equal
	     ~printer:Command.as_string
	     (Command.create 
		~sender:(Some "foo.bar") 
		~text:(Some "ta ta") 
		"PART" ["#foo"; "#bar"])
	     (Command.from_string ":foo.bar PART #foo #bar :ta ta");
	)
    ]
      

let regression_tests =
  let login_script nick =
    [
      Send ("USER " ^ nick ^ " +iw " ^ nick ^ " :gecos\r\n");
      Send ("NICK " ^ nick ^ "\r\n");
      Recv (":testserver.test 001 " ^ nick ^ " :Welcome to IRC.\r\n");
    ]
  in
    "Regression tests" >:::
      [
	"Simple connection" >::
	  (do_chat (login_script "nick"));
      ]

let _ =
  Irc.name := "testserver.test";
  run_test_tt_main (TestList [unit_tests; regression_tests])
	
  
