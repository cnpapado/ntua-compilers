# # OS type: Linux/Win DJGPP
# ifdef OS
#    EXE=.exe
# else
#    EXE=
# endif

# OCAMLC_FLAGS=-g # add debugging information while compiling and linking in order to debug with ocamldebug
# OCAMLC=ocamlc
# OCAMLDEP=ocamldep

# %.cmo: %.ml %.mli
# 	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

# %.cmi: %.mli
# 	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

# %.cmo %.cmi: %.ml
# 	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

# edsger$(EXE): lexer.cmo parser.cmo main.cmo
# 	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

# lexer.ml: lexer.mll
# 	ocamllex -o $@ $<

# parser.ml parser.mli: parser.mly
# 	ocamlyacc -v parser.mly

.PHONY: clean distclean only_lexer

# -include .depend

# depend: lexer.ml lexer.mli parser.ml parser.mli main.ml
# 	$(OCAMLDEP) $^ > .depend

edsger: lexer.cmo parser.cmo main.cmo
	ocamlc -o edsger lexer.cmo parser.cmo main.cmo

main.cmo: main.ml
	ocamlc -c main.ml

lexer.cmo: lexer.ml lexer.cmi parser.cmo parser.cmi
	ocamlc -c lexer.ml

lexer.cmi: lexer.mli parser.cmo parser.cmi
	ocamlc -c lexer.mli

ast.cmi: ast.mli Types.cmo
	ocamlc -c ast.mli

Types.cmo: Types.ml
	ocamlc -c Types.ml

parser.cmo: parser.ml parser.cmi ast.cmi
	ocamlc -c parser.ml

parser.cmi: parser.mli ast.cmi
	ocamlc -c parser.mli 

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean:
	$(RM) lexer.ml parser.ml parser.mli *.cmo *.cmi *~

distclean: clean
	$(RM) edsger lexer 

only_lexer: lexer #lexer.cmo lexer_main.cmo

lexer_main.cmo: lexer_main.ml
	ocamlc -c lexer_main.ml

lexer: lexer.cmo parser.cmo lexer_main.cmo
	ocamlc -o lexer lexer.cmo parser.cmo lexer_main.cmo