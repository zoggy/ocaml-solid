include ../master.Makefile

all: containers.js

containers.js: containers.ml
	$(OCAMLFIND) ocamlc -o containers.byte -g -rectypes -package solid.js -linkpkg $(COMPFLAGS) $^
	js_of_ocaml +nat.js +weak.js -o $@ containers.byte

clean:
	$(RM) *.byte containers.js *.cm* *.annot *.o