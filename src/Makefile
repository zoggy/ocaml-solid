#################################################################################
#                OCaml-IRI                                                      #
#                                                                               #
#    Copyright (C) 2016 Institut National de Recherche en Informatique          #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Lesser General Public License version        #
#    3 as published by the Free Software Foundation.                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU Lesser General Public           #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#                                                                               #
#################################################################################

include ../master.Makefile

OCAMLFIND=ocamlfind
COMPFLAGS=-annot -safe-string -g
OF_FLAGS=-package $(PACKAGES),$(JS_PACKAGES),$(TLS_PACKAGES)

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

LIB=solid.cmxa
LIB_CMXS=$(LIB:.cmxa=.cmxs)
LIB_A=$(LIB:.cmxa=.a)
LIB_BYTE=$(LIB:.cmxa=.cma)
LIB_CMI=$(LIB:.cmxa=.cmi)

LIB_CMXFILES= \
	ldp_types.cmx \
	ldp_http.cmx \
	solid_profile.cmx
#	solid_webidrsa.cmx

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)
LIB_OFILES=$(LIB_CMXFILES:.cmx=.o)

LIB_TLS=solid_tls.cmxa
LIB_TLS_CMXS=$(LIB_TLS:.cmxa=.cmxs)
LIB_TLS_A=$(LIB_TLS:.cmxa=.a)
LIB_TLS_BYTE=$(LIB_TLS:.cmxa=.cma)
LIB_TLS_CMI=$(LIB_TLS:.cmxa=.cmi)

LIB_TLS_CMXFILES= \
	ldp_tls.cmx \

LIB_TLS_CMOFILES=$(LIB_TLS_CMXFILES:.cmx=.cmo)
LIB_TLS_CMIFILES=$(LIB_TLS_CMXFILES:.cmx=.cmi)
LIB_TLS_OFILES=$(LIB_TLS_CMXFILES:.cmx=.o)

LIB_JS=solid_js.cma
LIB_JS_BYTE=$(LIB_JS)
LIB_JS_CMI=$(LIB_JS:.cma=.cmi)

LIB_JS_CMOFILES= \
	$(LIB_CMOFILES) \
	ldp_js.cmo \
	solid_updates.cmo
LIB_JS_CMIFILES=$(LIB_JS_CMOFILES:.cmo=.cmi)

all: byte opt
byte: $(LIB_BYTE) $(LIB_TLS_BYTE) $(LIB_JS)
opt: $(LIB) $(LIB_CMXS) $(LIB_TLS) $(LIB_TLS_CMXS)

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt -o $@ -a $(LIB_CMXFILES)

$(LIB_CMXS): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt -shared -o $@ $(LIB_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLFIND) ocamlc -o $@ -a $(LIB_CMOFILES)

$(LIB_TLS): $(LIB_TLS_CMIFILES) $(LIB_TLS_CMXFILES)
	$(OCAMLFIND) ocamlopt -o $@ -a $(LIB_TLS_CMXFILES)

$(LIB_TLS_CMXS): $(LIB_TLS_CMIFILES) $(LIB_TLS_CMXFILES)
	$(OCAMLFIND) ocamlopt -shared -o $@ $(LIB_TLS_CMXFILES)

$(LIB_TLS_BYTE): $(LIB_TLS_CMIFILES) $(LIB_TLS_CMOFILES)
	$(OCAMLFIND) ocamlc -o $@ -a $(LIB_TLS_CMOFILES)

$(LIB_JS): $(LIB_JS_CMIFILES) $(LIB_JS_CMOFILES)
	$(OCAMLFIND) ocamlc -o $@ -a $(LIB_JS_CMOFILES)

##########
.PHONY: doc
dump.odoc:
	$(OCAMLFIND) ocamldoc -package $(JS_PACKAGES) -dump $@ -rectypes \
	solid_*.mli

doc: dump.odoc
	$(MKDIR) doc
	$(OCAMLFIND) ocamldoc -load $^ -t Solid -d doc -html

docstog: dump.odoc
	$(MKDIR) web/refdoc
	ocamldoc.opt \
	-t "SOLID reference documentation" \
	-load $^ -d web/refdoc -i `ocamlfind query stog` -g odoc_stog.cmxs

##########
install: byte opt
	$(OCAMLFIND) install solid META ../LICENSE \
		$(LIB) $(LIB_CMXS) $(LIB_OFILES) $(LIB_CMXFILES) $(LIB_A) \
		$(LIB_BYTE) $(LIB_CMIFILES) $(LIB_CMIFILES:.cmi=.mli) \
		$(LIB_TLS) $(LIB_TLS_CMXS) $(LIB_TLS_OFILES) $(LIB_TLS_CMXFILES) $(LIB_TLS_A) \
		$(LIB_TLS_BYTE) $(LIB_TLS_CMIFILES) $(LIB_TLS_CMIFILES:.cmi=.mli) \
		$(LIB_JS) $(LIB_JS_CMIFILES) $(LIB_JS_CMIFILES:.cmi=.mli)

uninstall:
	ocamlfind remove solid

# archive :
###########
archive:
	git archive --prefix=solid-$(VERSION)/ HEAD | gzip > ../solid-gh-pages/xtmpl-$(VERSION).tar.gz

#####
clean:
	$(RM) *.cm* *.o *.annot *.a dump.odoc

# headers :
###########
HEADFILES=Makefile *.ml *.mli
.PHONY: headers noheaders
headers:
	headache -h header -c .headache_config $(HEADFILES)

noheaders:
	headache -r -c .headache_config $(HEADFILES)

# depend :
##########

.PHONY: depend

.depend depend:
	$(OCAMLFIND) ocamldep *.ml *.mli  > .depend

include .depend
