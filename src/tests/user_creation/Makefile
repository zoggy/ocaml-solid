include ../Makefile.common

test: toto tutu

client: client.ml
	@$(OCAMLFIND) ocamlopt -o $@ -package solid.tls,lwt.ppx -rectypes -linkpkg $<

toto:
	@$(RM) -r www ; $(CP) -r origin www ; \
	$(LAUNCH_SERVER) --add-user $(URL)toto \
	--name "Toto Totoro" \
	--cert-label "Toto's self-signed cert" \
	--pem toto.pem \
	--profile && \
	diff -r ref www

tutu: client
	@$(RM) -r www ; $(CP) -r origin www ; \
	$(LAUNCH_SERVER) --add-user $(URL)tutu \
	--name "Tutu Turlututu" \
	--cert-label "Tutu's self-signed cert" \
	--create-cert tutu \
	--profile && \
	( $(LAUNCH_SERVER) & sleep 1 ; \
	./client $(URL)tutu/Private/ ; export RES=$$? ; kill $$! ; exit $${RES} )


