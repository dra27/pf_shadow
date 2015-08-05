# ################################################################################################ #
# MetaStack Solutions Ltd.                                                                         #
# ################################################################################################ #
# Objective Caml Shadowed Variable Warning Tool                                                    #
# ################################################################################################ #
# Copyright (c) 2008-9 MetaStack Solutions Ltd.                                                    #
# All rights reserved.                                                                             #
# ################################################################################################ #
# Author: David Allsopp                                                                            #
# 13-Sep-2008                                                                                      #
# ################################################################################################ #
# Redistribution and use in source and binary forms, with or without modification, are permitted   #
# provided that the following two conditions are met:                                              #
#     1. Redistributions of source code must retain the above copyright notice, this list of       #
#        conditions and the following disclaimer.                                                  #
#     2. Neither the name of MetaStack Solutions Ltd. nor the names of its contributors may be     #
#        used to endorse or promote products derived from this software without specific prior     #
#        written permission.                                                                       #
#                                                                                                  #
# This software is provided by the Copyright Holder 'as is' and any express or implied warranties, #
# including, but not limited to, the implied warranties of merchantability and fitness for a       #
# particular purpose are disclaimed. In no event shall the Copyright Holder be liable for any      #
# direct, indirect, incidental, special, exemplary, or consequential damages (including, but not   #
# limited to, procurement of substitute goods or services; loss of use, data, or profits; or       #
# business interruption) however caused and on any theory of liability, whether in contract,       #
# strict liability, or tort (including negligence or otherwise) arising in any way out of the use  #
# of this software, even if advised of the possibility of such damage.                             #
# ################################################################################################ #

OPT=1
OCAMLC=ocamlc.exe
EXE=checkShadow.exe

ifeq ($(OPT), 0)
	OBJ=cmo
	TOOL=ocamlc
else
	OBJ=cmx
	TOOL=ocamlopt
endif

BINDIR=$(shell which $(OCAMLC) | sed -e "s/\\/\\\\/g" | xargs dirname)

all: pf_shadow.cmo $(EXE)

$(EXE): checkShadow.$(OBJ)
	ocamlfind $(TOOL) -o $@ -package unix -linkpkg checkShadow.$(OBJ)

checkShadow.$(OBJ): checkShadow.ml
	ocamlfind $(TOOL) -c -dtypes -package unix checkShadow.ml

checkShadow.ml: checkShadow.mll
	ocamllex checkShadow.mll

pf_shadow.cmo: pf_shadow.ml
	ocamlfind ocamlc -dtypes -syntax camlp4o -package camlp4.lib,camlp4.quotations -c pf_shadow.ml

install: pf_shadow.cmo $(EXE)
	ocamlfind install pf_shadow pf_shadow.cmo META
	cp $(EXE) $(BINDIR)/$(EXE)

remove:
	ocamlfind remove pf_shadow
	rm -f $(BINDIR)/$(EXE)

clean:
	rm -f checkShadow.annot checkShadow.cmi checkShadow.cmo checkShadow.cmx checkShadow.o checkShadow.ml $(EXE) pf_shadow.annot pf_shadow.cmi pf_shadow.cmo
