CFLAGS = -Wall -Werror
TARGETS = bot
TARGETS += infobot

all: $(TARGETS)

infobot: infobot.o cdb.o cdbmake.o

.PHONY: clean
clean:
	rm -f $(TARGETS) *.o
