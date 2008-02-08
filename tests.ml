open Unixqueue
open OUnit
open Chat

let do_chat script () =
  let irc_instance ues fd =
    let irc = new Irc.irc ues in
      irc#set_fd fd "nick" "gecos";
      irc#debug true
  in
    chat script irc_instance

let normal_tests =
  let login_script =
    [
      Recv "USER nick +iw nick :gecos\n";
      Recv "NICK nick\n";
      Send ":testserver.test NOTICE nick :*** Hi there.\n";
      Send "PING :12345\n";
      Recv "PONG :12345\n";
    ]
  in
    "Normal tests" >:::
      [
	"Simple connection" >::
	  (do_chat
	     login_script);

	"Full connection" >::
	  (do_chat
	     ([Send ":testserver.test NOTICE AUTH :*** Doing some pointless ident junk...\n"] @
		login_script @
		[
		  Send ":testserver.test 001 nick :Welcome to the test script\n";
		  Send ":testserver.test 002 nick :Your host is testserver.test\n";
		  Send ":testserver.test 003 nick :This server is timeless\n";
		  Send ":testserver.test 004 nick testserver.test testscript DGabcdfg bilmnopst bkloveI\n";
		  Send ":testserver.test 005 nick CALLERID CASEMAPPING=rfc1459 KICKLEN=160 MODES=4 WHATEVER=4 WHO=1 CARES=3 :are supported by this server\n";
		  Send ":testserver.test 043 00XAAAAL6 :your unique ID\n";
		  Send ":testserver.test 251 nick :There are 14 users and 4 invisible on 1 servers\n";
		  Send ":testserver.test 252 nick 1 :IRC Operators online\n";
		  Send ":testserver.test 254 4 :channels formed\n";
		  Send ":testserver.test 255 nick :I have 17 clients and 0 servers\n";
		  Send ":testserver.test 265 nick :Current local users: 17  Max: 25\n";
		  Send ":testserver.test 266 nick :Current global users: 17  Max: 25\n";
		  Send ":testserver.test 250 nick :Highest connection count: 25 (25 clients) (430 connections received)\n";
		  Send ":testserver.test 375 nick :- xirc.lanl.gov Message of the Day -\n";
		  Send ":testserver.test 372 nick :- This is ircd-hybrid MOTD replace it with something better\n";
		  Send ":testserver.test 376 nick :End of /MOTD command.\n";
		]
	     ));
      ]

let _ =
  run_test_tt_main (TestList [normal_tests])
	
  
