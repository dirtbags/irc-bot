#! /usr/bin/python

##
## Example of an infobot, using the provided helper.
##

import sys
import os
from subprocess import *

db = "infobot.cdb"

command = os.environ.get("command")
sender = os.environ.get("sender")
forum = os.environ.get("forum")
text = os.environ.get("text")

def factoids(*args):
    cmd = ["./factoids"]
    cmd.extend(args)
    p = Popen(cmd, stdout=PIPE, stderr=STDOUT)
    for bline in p.stdout:
        line = bline.decode('utf-8')[:-1]
        print("PRIVMSG %s :%s" % (forum, line))

# Create db if it doesn't exist
if not os.path.exists(db):
    factoids("-n", db)

if command == "_INIT_":
    print("NICK infotest")
    print("USER infotest infotest infotest: Infobot example")
elif command == "001":
    print("JOIN #infotest")
elif command == "PRIVMSG":
    if text[0] == "!":
        txt = text[1:]
        if " += " in txt:
            key, val = txt.split(' += ', 1)
            factoids("-a", val, db, key)
        elif " -= " in txt:
            key, glob = txt.split(' -= ', 1)
            factoids("-r", glob, db, key)
        else:
            factoids("-l", db, txt)
    else:
        factoids(db, text)

