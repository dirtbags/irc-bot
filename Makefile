OCS_VERSION = 1.0.3
OCS_DIR = ocs-$(OCS_VERSION)


INCLUDES = -I $(OCS_DIR)/src
OCAMLFLAGS = $(INCLUDES)
OCAMLOPT = ocamlopt
OCAMLC = ocamlc
OCAMLDEP = ocamldep $(INCLUDES)
OCAMLLIBS = unix.cma str.cma nums.cma

bot: irc.cmo dispatch.cmo command.cmo iobuf.cmo cdb.cmo bot.cmo
bot: $(OCS_DIR)/src/ocs.cma
	$(OCAMLC) -o $@ $(OCAMLLIBS) $^

$(OCS_DIR)/src/ocs.cma $(OCS_DIR)/src/ocs.cmxa: $(OCS_DIR)
	cd $(OCS_DIR)/src && make

$(OCS_DIR): ocs-$(OCS_VERSION).tar.gz
	tar xzf $<

ocs-$(OCS_VERSION).tar.gz:
	wget http://will.iki.fi/software/ocs/files/$@

.PHONY: clean
clean:
	rm -f bot *.cm* *.o

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

depend: .depend
.depend: *.mli *.ml
	$(OCAMLDEP) $(INCLUDES) $^ > $@

include .depend
