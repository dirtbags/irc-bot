CFLAGS = -Wall -Werror
TARGETS = bot factoids

all: $(TARGETS)

%: src/%
	cp $< $@

src/bot:
src/factoids: src/factoids.o src/cdb.o src/cdbmake.o

.PHONY: clean
clean:
	rm -f $(TARGETS) $(addprefix src/, $(TARGETS)) src/*.o
