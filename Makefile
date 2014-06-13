CFLAGS = -Wall -Werror
TARGETS = bot factoids slack.cgi

all: $(TARGETS)

%: src/%
	cp $< $@

src/bot:
src/factoids: src/factoids.o src/cdb.o src/cdbmake.o

src/slack.cgi: src/slack.cgi.o src/cgi.o

.PHONY: clean
clean:
	rm -f $(TARGETS) $(addprefix src/, $(TARGETS)) src/*.o
