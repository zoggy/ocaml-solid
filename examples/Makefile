include ../master.Makefile

all: solid_ls solid_get solid_put solid_delete containers.js

containers.js: ldp_containers.cmo containers.ml
	$(OCAMLFIND) ocamlc -o containers.byte -g -package solid.js -linkpkg $(COMPFLAGS) $^
	js_of_ocaml +nat.js +weak.js -o $@ containers.byte

OF_FLAGS=-package solid.tls

solid_ls: solid_common.cmx ldp_containers.cmx solid_ls.cmx
	$(OCAMLFIND) ocamlopt -o $@ -package solid.tls,solid.curl -linkpkg $^

solid_get: solid_common.cmx solid_get.cmx
	$(OCAMLFIND) ocamlopt -o $@ -package solid.tls,solid.curl -linkpkg $^

solid_put: solid_common.cmx solid_put.cmx
	$(OCAMLFIND) ocamlopt -o $@ -package solid.tls,solid.curl -linkpkg $^

solid_delete: solid_common.cmx solid_delete.cmx
	$(OCAMLFIND) ocamlopt -o $@ -package solid.tls,solid.curl -linkpkg $^


clean:
	$(RM) solid_ls solid_get solid_put solid_delete
	$(RM) *.byte containers.js *.cm* *.annot *.o