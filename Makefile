INCLUDES = 
OCAMLFLAGS = $(INCLUDES)
OCAMLOPT = ocamlopt
OCAMLC = ocamlc -g
OCAMLDEP = ocamldep $(INCLUDES)
OCAMLLIBS = unix.cma str.cma nums.cma

bot: irc.cmo dispatch.cmo process.cmo command.cmo iobuf.cmo bot.cmo
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
