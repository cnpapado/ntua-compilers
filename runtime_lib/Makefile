CC := clang
CFLAGS := -c
AR := ar
ARFLAGS := -cvq

lib.a: lib.o
	$(AR) $(ARFLAGS) $@ $^

lib.o: lib.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	$(RM) -f lib.o

distclean: clean
	$(RM) -f lib.a
