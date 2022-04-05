# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

OCAMLC_FLAGS=-g
OCAMLC=ocamlc

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

minibasic$(EXE): lexer.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

lexer.ml: lexer.mll
	ocamllex -o $@ $<

.PHONY: clean distclean

clean:
	$(RM) lexer.ml *.cmo *.cmi *~ minibasic$(EXE)

run:
	ocamlrun minibasic
#distclean: clean
#	$(RM) minibasic$(EXE)
