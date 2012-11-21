CFLAGS = -Wall -Werror
TARGETS = bot
TARGETS += extras/factoids

all: $(TARGETS)

extras/factoids: extras/factoids.o extras/cdb.o extras/cdbmake.o

.PHONY: clean
clean:
	rm -f $(TARGETS) *.o extras/*.o
