OCAML_PACKAGES := -package equeue -package pcre

TARGETS = ircd

OCAMLOPTS = -g
OCAMLC = ocamlfind c $(OCAMLOPTS)

all: $(TARGETS)

ircd: connection.cmo ircd.cmo

.PHONY: test
test: tests
	./tests

##
## Generic ways to do things
##
%.cmo : %.ml
	$(OCAMLC) -c $^ $(OCAML_PACKAGES)

%: %.cmo
	$(OCAMLC) -o $@ $(filter-out $<, $^) $< $(OCAML_PACKAGES) -linkpkg

##
## 
##
include .deps
.deps: *.ml
	ocamldep $^ > $@

.PHONY: clean
clean:
	rm -f $(TARGETS) *.cm? *.o
