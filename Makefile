default:
	dune build
	# cp _build/install/default/bin/gracec gracec

clean:
	dune clean

distclean: clean
	$(RM) gracec