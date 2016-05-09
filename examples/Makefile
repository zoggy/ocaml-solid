include ../master.Makefile

all: containers.js tls_ls

containers.js: ldp_containers.cmo containers.ml
	$(OCAMLFIND) ocamlc -o containers.byte -g -package solid.js -linkpkg $(COMPFLAGS) $^
	js_of_ocaml +nat.js +weak.js -o $@ containers.byte

OF_FLAGS=-package solid.tls

tls_ls: tls_common.cmx ldp_containers.cmx tls_ls.cmx
	$(OCAMLFIND) ocamlopt -o $@ -package solid.tls -linkpkg $^


clean:
	$(RM) *.byte containers.js *.cm* *.annot