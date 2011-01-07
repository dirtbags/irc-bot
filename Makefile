CFLAGS = -Wall -Werror
TARGETS = dispatch irc

all: $(TARGETS)

.PHONY: clean
clean:
	rm -f $(TARGETS) *.o
