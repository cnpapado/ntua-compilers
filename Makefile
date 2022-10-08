# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

OCAMLC_FLAGS=-g
OCAMLC=ocamlc
OCAMLDEP=ocamldep

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

minibasic$(EXE): lexer.cmo Parser.cmo main.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

lexer.ml: lexer.mll
	ocamllex -o $@ $<

Parser.ml Parser.mli: Parser.mly
	ocamlyacc -v Parser.mly

.PHONY: clean distclean

-include .depend

depend: lexer.ml lexer.mli Parser.ml Parser.mli main.ml
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) lexer.ml Parser.ml Parser.mli Parser.output *.cmo *.cmi *~

distclean: clean
	$(RM) minibasic$(EXE) .depend
