CFLAGS = -Wall -Werror
TARGETS = bot

all: $(TARGETS)

.PHONY: clean
clean:
	rm -f $(TARGETS) *.o
