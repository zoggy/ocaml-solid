#################################################################################
#                OCaml-Solid                                                    #
#                                                                               #
#    Copyright (C) 2016-2017 Institut National de Recherche en Informatique     #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Lesser General Public License version        #
#    3 as published by the Free Software Foundation.                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU General Public License for more details.                               #
#                                                                               #
#    You should have received a copy of the GNU General Public License          #
#    along with this program; if not, write to the Free Software                #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

include ../master.Makefile

OCAMLFIND=ocamlfind
COMPFLAGS=-annot -safe-string -g -rectypes
OF_FLAGS=-package $(PACKAGES),$(JS_PACKAGES),$(TLS_PACKAGES),$(CURL_PACKAGES)
OF_FLAGS_SERVER=$(OF_FLAGS) -package $(SERVER_PACKAGES)
OF_FLAGS_COMP=$(OF_FLAGS_SERVER)

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

COMMON_CMXFILES= \
	ldp_misc.cmx \
	ldp_log.cmx \
	ldp_types.cmx \
	ldp_http.cmx \
	ldp_cookies.cmx \
	ldp_containers.cmx \
	solid_profile.cmx
#	solid_webidrsa.cmx

COMMON_CMOFILES=$(COMMON_CMXFILES:.cmx=.cmo)
COMMON_CMIFILES=$(COMMON_CMXFILES:.cmx=.cmi)
COMMON_OFILES=$(COMMON_CMXFILES:.cmx=.o)

LIB=solid.cmxa
LIB_CMXS=$(LIB:.cmxa=.cmxs)
LIB_A=$(LIB:.cmxa=.a)
LIB_BYTE=$(LIB:.cmxa=.cma)
LIB_CMI=$(LIB:.cmxa=.cmi)

LIB_CMXFILES= \
	$(COMMON_CMXFILES) \
	ldp_cache.cmx

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)
LIB_OFILES=$(LIB_CMXFILES:.cmx=.o)

LIB_TLS=solid_tls.cmxa
LIB_TLS_CMXS=$(LIB_TLS:.cmxa=.cmxs)
LIB_TLS_A=$(LIB_TLS:.cmxa=.a)
LIB_TLS_BYTE=$(LIB_TLS:.cmxa=.cma)
LIB_TLS_CMI=$(LIB_TLS:.cmxa=.cmi)

LIB_TLS_CMXFILES= \
	ldp_tls.cmx

LIB_TLS_CMOFILES=$(LIB_TLS_CMXFILES:.cmx=.cmo)
LIB_TLS_CMIFILES=$(LIB_TLS_CMXFILES:.cmx=.cmi)
LIB_TLS_OFILES=$(LIB_TLS_CMXFILES:.cmx=.o)

LIB_CURL=solid_curl.cmxa
LIB_CURL_CMXS=$(LIB_CURL:.cmxa=.cmxs)
LIB_CURL_A=$(LIB_CURL:.cmxa=.a)
LIB_CURL_BYTE=$(LIB_CURL:.cmxa=.cma)
LIB_CURL_CMI=$(LIB_CURL:.cmxa=.cmi)

LIB_CURL_CMXFILES= \
	ldp_curl.cmx \

LIB_CURL_CMOFILES=$(LIB_CURL_CMXFILES:.cmx=.cmo)
LIB_CURL_CMIFILES=$(LIB_CURL_CMXFILES:.cmx=.cmi)
LIB_CURL_OFILES=$(LIB_CURL_CMXFILES:.cmx=.o)

LIB_JS=solid_js.cma
LIB_JS_BYTE=$(LIB_JS)
LIB_JS_CMI=$(LIB_JS:.cma=.cmi)

LIB_JS_CMOFILES= \
	$(COMMON_CMOFILES) \
	ldp_js.cmo \
	solid_updates.cmo
LIB_JS_CMIFILES=$(LIB_JS_CMOFILES:.cmo=.cmi)

LIB_TOOL=solid_tool.cmxa
LIB_TOOL_CMXS=$(LIB_TOOL:.cmxa=.cmxs)
LIB_TOOL_A=$(LIB_TOOL:.cmxa=.a)
LIB_TOOL_BYTE=$(LIB_TOOL:.cmxa=.cma)
LIB_TOOL_CMI=$(LIB_TOOL:.cmxa=.cmi)

LIB_TOOL_CMXFILES= \
	solid_tool_common.cmx \

LIB_TOOL_CMOFILES=$(LIB_TOOL_CMXFILES:.cmx=.cmo)
LIB_TOOL_CMIFILES=$(LIB_TOOL_CMXFILES:.cmx=.cmi)
LIB_TOOL_OFILES=$(LIB_TOOL_CMXFILES:.cmx=.o)

TOOLS=solid_ls solid_get solid_put solid_delete

SERVER=oss
SERVER_BYTE=$(SERVER).byte
SERVER_CMXFILES=\
	server_log.cmx \
	server_conf.cmx \
	cohttp_tls.cmx \
	server_auth.cmx \
	server_http_tls.cmx \
	server_page.cmx \
	server_fs_route.cmx \
	server_fs.cmx \
	server_acl.cmx \
	server_webmachine.cmx \
	server_openssl.cmx \
	server_user.cmx \
	server_main.cmx
SERVER_CMIFILES=$(SERVER_CMXFILES:.cmx=.cmi)
SERVER_CMOFILES=$(SERVER_CMXFILES:.cmx=.cmo)

all: opt byte
byte: $(LIB_BYTE) $(LIB_TLS_BYTE) $(LIB_CURL_BYTE) $(LIB_TOOL_BYTE) $(LIB_JS) \
	$(SERVER_BYTE)
opt: $(LIB) $(LIB_CMXS) $(LIB_TLS) $(LIB_TLS_CMXS) \
	$(LIB_CURL) $(LIB_CURL_CMXS) $(LIB_TOOL) $(LIB_TOOL_CMXS) tools $(SERVER)
tools:$(TOOLS)
server: $(SERVER)

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

$(LIB_CURL): $(LIB_CURL_CMIFILES) $(LIB_CURL_CMXFILES)
	$(OCAMLFIND) ocamlopt -o $@ -a $(LIB_CURL_CMXFILES)

$(LIB_CURL_CMXS): $(LIB_CURL_CMIFILES) $(LIB_CURL_CMXFILES)
	$(OCAMLFIND) ocamlopt -shared -o $@ $(LIB_CURL_CMXFILES)

$(LIB_CURL_BYTE): $(LIB_CURL_CMIFILES) $(LIB_CURL_CMOFILES)
	$(OCAMLFIND) ocamlc -o $@ -a $(LIB_CURL_CMOFILES)

$(LIB_TOOL): $(LIB_CURL) $(LIB_TLS) $(LIB_TOOL_CMIFILES) $(LIB_TOOL_CMXFILES)
	$(OCAMLFIND) ocamlopt -o $@ -a $(LIB_TOOL_CMXFILES)

$(LIB_TOOL_CMXS): $(LIB_CURL) $(LIB_TLS) $(LIB_TOOL_CMIFILES) $(LIB_TOOL_CMXFILES)
	$(OCAMLFIND) ocamlopt -shared -o $@ $(LIB_TOOL_CMXFILES)

$(LIB_TOOL_BYTE): $(LIB_CURL_BYTE) $(LIB_TLS_BYTE) $(LIB_TOOL_CMIFILES) $(LIB_TOOL_CMOFILES)
	$(OCAMLFIND) ocamlc -o $@ -a $(LIB_TOOL_CMOFILES)

$(LIB_JS): $(LIB_JS_CMIFILES) $(LIB_JS_CMOFILES)
	$(OCAMLFIND) ocamlc -o $@ -a $(LIB_JS_CMOFILES)

solid_ls: $(LIB) $(LIB_CURL) $(LIB_TLS) $(LIB_TOOL) solid_tool_ls.cmx
	$(OCAMLFIND) ocamlopt -o $@ $(OF_FLAGS) -linkpkg $^

solid_get: $(LIB) $(LIB_CURL) $(LIB_TLS) $(LIB_TOOL) solid_tool_get.cmx
	$(OCAMLFIND) ocamlopt -o $@ $(OF_FLAGS) -linkpkg $^

solid_put: $(LIB) $(LIB_CURL) $(LIB_TLS) $(LIB_TOOL) solid_tool_put.cmx
	$(OCAMLFIND) ocamlopt -o $@ $(OF_FLAGS) -linkpkg $^

solid_delete: $(LIB) $(LIB_CURL) $(LIB_TLS) $(LIB_TOOL) solid_tool_delete.cmx
	$(OCAMLFIND) ocamlopt -o $@ $(OF_FLAGS) -linkpkg $^

$(SERVER): $(LIB) $(LIB_CURL) $(LIB_TLS) $(LIB_TOOL) \
	$(SERVER_CMIFILES) $(SERVER_CMXFILES)
	$(OCAMLFIND) ocamlopt -o $@ $(OF_FLAGS_SERVER) -linkpkg \
	$(LIB) $(LIB_CURL) $(LIB_TLS) $(LIB_TOOL) $(SERVER_CMXFILES)

$(SERVER_BYTE): $(LIB_BYTE) $(LIB_CURL_BYTE) $(LIB_TLS_YTE) $(LIB_TOOL_BYTE) \
	$(SERVER_CMIFILES) $(SERVER_CMOFILES)
	$(OCAMLFIND) ocamlc -o $@ $(OF_FLAGS_SERVER) -linkpkg \
	$(LIB_BYTE) $(LIB_CURL_BYTE) $(LIB_TLS_BYTE) $(LIB_TOOL_BYTE) \
	$(SERVER_CMOFILES)

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
		$(LIB_CURL) $(LIB_CURL_CMXS) $(LIB_CURL_OFILES) $(LIB_CURL_CMXFILES) $(LIB_CURL_A) \
		$(LIB_CURL_BYTE) $(LIB_CURL_CMIFILES) $(LIB_CURL_CMIFILES:.cmi=.mli) \
		$(LIB_TOOL) $(LIB_TOOL_CMXS) $(LIB_TOOL_OFILES) $(LIB_TOOL_CMXFILES) $(LIB_TOOL_A) \
		$(LIB_TOOL_BYTE) $(LIB_TOOL_CMIFILES) $(LIB_TOOL_CMIFILES:.cmi=.mli) \
		$(LIB_JS) $(LIB_JS_CMIFILES) $(LIB_JS_CMIFILES:.cmi=.mli) \
		$(TOOLS)

uninstall:
	ocamlfind remove solid

# archive :
###########
archive:
	git archive --prefix=solid-$(VERSION)/ HEAD | gzip > ../solid-gh-pages/xtmpl-$(VERSION).tar.gz

#####
clean:
	$(RM) *.cm* *.o *.annot *.a $(TOOLS) dump.odoc

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
	$(OCAMLFIND) ocamldep -package lwt.ppx *.ml *.mli  > .depend

include .depend

USER_TEMPLATES= \
	user_templates/Applications/,acl \
	user_templates/Inbox/,acl \
	user_templates/Private/,acl \
	user_templates/profile/card \
	user_templates/profile/card,acl \
	user_templates/Public/,acl \
	user_templates/settings/,acl \
	user_templates/settings/preferences.ttl \
	user_templates/Shared/,acl \
	user_templates/Work/,acl

server_user.cmx: $(USER_TEMPLATES)
server_user.cmo: $(USER_TEMPLATES)
