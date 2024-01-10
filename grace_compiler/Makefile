.PHONY: lib

default: lib
	dune build

lib:
	$(MAKE) -C ../runtime_lib

clean:
	dune clean
	$(MAKE) -C ../runtime_lib clean

distclean: clean
	$(RM) gracec.exe *.exe *.imm *.asm
	$(MAKE) -C ../runtime_lib distclean