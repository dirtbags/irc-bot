open Unixqueue
open OUnit
open Chat

let do_chat script () =
  let ircd_instance ues fd =
    let irc = new Ircd.ircd_connection ues fd in
      irc#debug true
  in
    chat script ircd_instance

let normal_tests =
  let login_script =
    [
      Send "USER nick +iw nick :gecos\n";
      Send "NICK nick\n";
      Recv ":testserver.test NOTICE nick :*** Hi there.\n";
      Recv "PING :12345\n";
      Send "PONG :12345\n";
    ]
  in
    "Normal tests" >:::
      [
	"Simple connection" >::
	  (do_chat
	     login_script);

	"Full connection" >::
	  (do_chat
	     (login_script @
		[
		  Recv ":testserver.test 001 nick :Welcome to the test script\n";
		  Recv ":testserver.test 002 nick :Your host is testserver.test\n";
		  Recv ":testserver.test 003 nick :This server is timeless\n";
		  Recv ":testserver.test 004 nick testserver.test testscript DGabcdfg bilmnopst bkloveI\n";
		  Recv ":testserver.test 005 nick CALLERID CASEMAPPING=rfc1459 KICKLEN=160 MODES=4 WHATEVER=4 WHO=1 CARES=3 :are supported by this server\n";
		  Recv ":testserver.test 043 00XAAAAL6 :your unique ID\n";
		  Recv ":testserver.test 251 nick :There are 14 users and 4 invisible on 1 servers\n";
		  Recv ":testserver.test 252 nick 1 :IRC Operators online\n";
		  Recv ":testserver.test 254 4 :channels formed\n";
		  Recv ":testserver.test 255 nick :I have 17 clients and 0 servers\n";
		  Recv ":testserver.test 265 nick :Current local users: 17  Max: 25\n";
		  Recv ":testserver.test 266 nick :Current global users: 17  Max: 25\n";
		  Recv ":testserver.test 250 nick :Highest connection count: 25 (25 clients) (430 connections received)\n";
		  Recv ":testserver.test 375 nick :- xirc.lanl.gov Message of the Day -\n";
		  Recv ":testserver.test 372 nick :- This is ircd-hybrid MOTD replace it with something better\n";
		  Recv ":testserver.test 376 nick :End of /MOTD command.\n";
		]
	     ));
      ]

let _ =
  run_test_tt_main (TestList [normal_tests])
	
  
