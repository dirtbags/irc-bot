CFLAGS = -Wall -Werror
TARGETS = dispatch irc-filter irc-esc
TARGETS += infobot

all: $(TARGETS)

infobot: infobot.o cdb.o

.PHONY: clean
clean:
	rm -f $(TARGETS) *.o
