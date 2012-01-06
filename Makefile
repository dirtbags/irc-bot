CFLAGS = -Wall -Werror
TARGETS = dispatch irc-filter

all: $(TARGETS)

.PHONY: clean
clean:
	rm -f $(TARGETS) *.o
