CFLAGS = -Wall -Werror
TARGETS = bot
TARGETS += infobot

all: $(TARGETS)

infobot: infobot.o cdb.o

.PHONY: clean
clean:
	rm -f $(TARGETS) *.o
