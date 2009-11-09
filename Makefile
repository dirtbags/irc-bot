INCLUDES = 
OCAMLFLAGS = $(INCLUDES)
OCAMLOPT = ocamlopt
OCAMLC = ocamlc
OCAMLDEP = ocamldep $(INCLUDES)
OCAMLLIBS = unix.cma str.cma nums.cma

bot: irc.cmo dispatch.cmo command.cmo iobuf.cmo cdb.cmo bindings.cmo plugin.cmo infobot.cmo bot.cmo infobot.cmo
	$(OCAMLC) -o $@ $(OCAMLLIBS) $^

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
