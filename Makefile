CFLAGS = -Wall -Werror
TARGETS = dispatch irc-filter irc-esc

all: $(TARGETS)

.PHONY: clean
clean:
	rm -f $(TARGETS) *.o
