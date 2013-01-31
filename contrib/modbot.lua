#! /usr/bin/lua

channel = "testchan"

--
-- A bot framework with a bunch of nifty features you can
-- call from your privmsg handler.
--

-- You should override these in your handler.  Don't change
-- them here, though, or you'll have to keep changing them
-- every time you install a new release.
bot = {}
bot.debug = false

bot.nick = channel .. "-moderator"
bot.user = "moderator"
bot.desc = "Channel Moderation Fairy"
bot.channels = {"#" .. channel, "#mod-" .. channel}
bot.triggers = {}

------------------------------------------------------------
-- Some convenience functions
--

function shesc(s)
	return "'" .. s:gsub("'", "'\\''") .. "'"
end

function syscmd(argv)
	local args = {}
	local cmdline
	local proc

	for _,a in ipairs(argv) do
		table.insert(args, shesc(a))
	end
	cmdline = table.concat(args, " ")

	proc = io.popen(cmdline, "r")
	return function () 
		return proc:read()
	end
end

function system(argv)
	local ret = {}

	for line in syscmd(argv) do
		table.insert(ret, line)
	end
	return ret
end


function string:startswith(needle)
	local len = needle:len()

	return self:sub(1, len) == needle
end

function string:endswith(needle)
	local len = needle:len()

	return self:sub(-len) == needle
end


------------------------------------------------------------
-- Bot object
--


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
	io.stderr:write(tostring(text) .. "\n")
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
		self:raw("PRIVMSG " .. self.forum .. " :Sending the rest in private")
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
-- Run something in the shell
--
function bot:syscmd(argv)
	for line in syscmd(argv) do
		if line:sub(1, 1) == ':' then
			self:emote(line:sub(2))
		else
			self:msg(line)
		end
	end
end

-- Process self.triggers
function bot:do_triggers()
	for pat,func in pairs(self.triggers) do
		local a1, a2, a3, a4, a5 = self.text:match(pat)

		if a1 and func then
			if type(func) == "string" then
				self:msg(func)
			else
				func(self, a1, a2, a3, a4, a5, a6)
			end
			return true
		end
	end
end

-- Use introspection to dispatch a command
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

-- Join channels
function bot:handle_001()
	self:raw("PRIVMSG nickserv :identify modbot4pescado.fleas")
end

-- Deal with nickname collision
function bot:handle_433()
	self:raw("NICK " .. self.nick .. (os.time() % 500))
end

-- Override this to handle messages
function bot:handle_privmsg()
	self:do_triggers()
end

-- Override this to handle any undefined command
function bot:handle_default()
end



---------------------------------
--
-- Moderation
--
--
require('lfs')

function queue()
	ret = {}
	for file in lfs.dir("queue") do
		if file:sub(1, 1) ~= "." then
			table.insert(ret, file)
		end
	end
	return ret
end

function push(text)
	now = os.time()
	f = io.open(("queue/%d"):format(now), "w")
	f:write(text)
	f:close()
end

function pop(q, peek)
	if not q then
		q = queue()
	end
	if #q == 0 then
		return
	end

	table.sort(q)
	fn = ("queue/%s"):format(q[1])

	f = io.open(fn)
	ret = f:read()
	f:close()

	if peek == nil then
		os.remove(fn)
		table.remove(q, 1)
	end

	return ret
end

function peek(q)
	return pop(q, true)
end

function bot:handle_notice()
	if (self.sender == 'NickServ') and self.text:startswith("You are successfully identified") then
		for _,chan in ipairs(self.channels) do
			self:raw("JOIN " .. chan)
		end
	end
end

function bot:handle_privmsg()
	if self.forum:sub(1, 1) == "#" then
		self:handle_public()
	else
		self:handle_private()
	end
end

function bot:handle_public()
	if self.forum:startswith("#mod-") then	
		q = queue()
		if self.text:startswith("!") then
			if self.text:startswith("!n") then
				question = pop(q)
				if not question then
					return self:msg("[Queue empty]")
				else
					self:raw("PRIVMSG " .. self.channels[1] .. " :" .. question)
				end
			elseif self.text:startswith("!d") then
				pop(q)
			end
					
			question = pop(q, true)
			if question then
				self:msg(("[1/%d] %s"):format(#q, question))
			else
				self:msg("[End of Queue]")
			end
		end
	end
end

function bot:handle_private()
	ok = false

	for addr in io.lines("authorized.txt") do
		if self.text:startswith(addr) then
			ok = true
		end
	end
	if not ok then
		return
	end

	q = queue()
	question = ("<%s> %s"):format(self.prefix, self.text)
	push(question)
	self:msg("Your question has been enqueued.  There are approximately " .. #q .. " questions before yours.")
	
	if (#q == 0) then
		self:raw("PRIVMSG " .. self.channels[2] .. " :[1/1] " .. question)
	end
end

bot.debug = true
bot:run()

