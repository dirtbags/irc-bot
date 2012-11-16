--
-- Bot object
--
-- require() this file, and you get a `bot` object with everything
-- you need to run a bot.  You can either switch on bot.command,
-- or invoke bot:run(), which will run self:handle_$COMMAND.
--
bot = {}
bot.debug = false

bot.nick = "newmont"
bot.user = "WoozleBot"
bot.desc = "A woozle.org bot"

bot.prefix = os.getenv("prefix")
bot.forum = os.getenv("forum")
bot.sender = os.getenv("sender")
bot.command = os.getenv("command")
bot.text = os.getenv("text")
bot.args = arg

--
-- Log to stderr
--
function bot:log(text)
    io.stderr:write(text .. "\n")
end

--
-- Log what we are working with
--
function bot:debug_input(text)
    self:log(("<   %-8s %8s/%-8s :%s"):format(
                   self.command,
                   self.sender or "-",
                   self.forum or "-",
                   self.text or ""))
end

--
-- Send a raw IRC command
--
function bot:raw(text)
    if self.debug then
        self:log("  > " .. text)
    end
    print(text)
end

--
-- Send a message to the forum, falling back to sender if we're
-- spamming the channel.
--
function bot:msg(text)
    self.msgs_sent = self.msgs_sent + 1
    if ((self.msgs_sent == 5) and (self.forum ~= self.sender)) then
        self:raw("PRIVMSG " .. forum .. " :Sending the rest in private")
        self.msg_recip = self.sender
    end
    self:raw("PRIVMSG " .. self.msg_recip .. " :" .. text)
end
bot.msgs_sent = 0
bot.msg_recip = bot.forum

--
-- Emote, like "* botname explodes"
--
function bot:emote(text)
    self:msg("\001ACTION " .. text .. "\001")
end

--
-- Use introspection to dispatch a command
--
function bot:run()
    local func = self["handle_" .. self.command:lower()]

    if (self.debug) then
        self:debug_input()
    end

    if (func) then
        func(self)
    else
        self:handle_default()
    end
end

-- Log in to IRC
function bot:handle__init_()
    self:raw("NICK " .. self.nick)
    self:raw(("USER %s %s %s :%s"):format(self.user, self.user, self.user, self.desc))
end

-- Deal with nickname collision
function bot:handle_433()
    self.raw("NICK " .. self.nick .. (os.time() % 500))
end

-- Override this to handle messages
function bot:handle_privmsg()
    self:log(("%s/%s: %s"):format(self.sender, self.forum, self.text))
end

-- Override this to handle any undefined command
function bot:handle_default()
end
