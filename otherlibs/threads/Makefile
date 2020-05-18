LIBNAME=threads
COBJS=externals.$(O)
CAMLOBJS=thread.cmo mutex.cmo condition.cmo event.cmo

include ../Makefile.otherlibs.common

LIBS = -nostdlib -I $(ROOTDIR)/stdlib -I $(ROOTDIR)/otherlibs/$(UNIXLIB)

CAMLC=$(BEST_OCAMLC) $(LIBS)
CAMLOPT=$(BEST_OCAMLOPT) $(LIBS)

thread.cmo: thread.cmi
thread.cmx: thread.cmi

.PHONY: depend
depend:
ifeq "$(TOOLCHAIN)" "msvc"
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
	$(CC) -MM $(OC_CPPFLAGS) *.c | sed -e 's/\.o/.$$(O)/g' > .depend
	$(CAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash *.mli *.ml >> .depend
endif

include .depend
