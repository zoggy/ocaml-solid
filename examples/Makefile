include ../master.Makefile

all: containers.js tls_ls tls_get tls_put

containers.js: ldp_containers.cmo containers.ml
	$(OCAMLFIND) ocamlc -o containers.byte -g -package solid.js -linkpkg $(COMPFLAGS) $^
	js_of_ocaml +nat.js +weak.js -o $@ containers.byte

OF_FLAGS=-package solid.tls

tls_ls: tls_common.cmx ldp_containers.cmx tls_ls.cmx
	$(OCAMLFIND) ocamlopt -o $@ -package solid.tls,solid.curl -linkpkg $^

tls_get: tls_common.cmx tls_get.cmx
	$(OCAMLFIND) ocamlopt -o $@ -package solid.tls,solid.curl -linkpkg $^

tls_put: tls_common.cmx tls_put.cmx
	$(OCAMLFIND) ocamlopt -o $@ -package solid.tls,solid.curl -linkpkg $^


clean:
	$(RM) *.byte containers.js *.cm* *.annot