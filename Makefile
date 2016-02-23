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

# DO NOT FORGET TO UPDATE META FILE
VERSION=0.1.0

OCAMLFIND=ocamlfind
PACKAGES=js_of_ocaml.ppx,rdf.js,lwt.ppx
COMPFLAGS=-annot -safe-string -g
OCAMLPP=

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

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)
LIB_OFILES=$(LIB_CMXFILES:.cmx=.o)

all: byte opt
byte: $(LIB_BYTE)
opt: $(LIB) $(LIB_CMXS)

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt -o $@ -a -package $(PACKAGES) $(LIB_CMXFILES)

$(LIB_CMXS): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt -shared -o $@ -package $(PACKAGES) $(LIB_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLFIND) ocamlc -o $@ -a -package $(PACKAGES) $(LIB_CMOFILES)

%.cmx: %.ml %.cmi
	$(OCAMLFIND) ocamlopt -c -package $(PACKAGES) $(COMPFLAGS) $<

%.cmo: %.ml %.cmi
	$(OCAMLFIND) ocamlc -c -package $(PACKAGES) $(COMPFLAGS) $<

%.cmi: %.mli
	$(OCAMLFIND) ocamlc -c -package $(PACKAGES) $(COMPFLAGS) $<

# Example
#########
examples: examples/containers.js

examples/containers.js: $(LIB_BYTE) examples/containers.ml
	$(OCAMLFIND) ocamlc -o examples/containers.byte -g -package rdf.js,js_of_ocaml.ppx,lwt,lwt.ppx,d3  -linkpkg $^
	js_of_ocaml -o $@ examples/containers.byte

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
	$(OCAMLFIND) install solid META LICENSE \
		$(LIB) $(LIB_CMXS) $(LIB_OFILES) $(LIB_CMXFILES) $(LIB_A) \
		$(LIB_BYTE) $(LIB_CMIFILES) solid.mli

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
