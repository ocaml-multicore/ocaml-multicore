ROOTDIR=../..

include $(ROOTDIR)/config/Makefile

LIBNAME=threads

LIBS=-nostdlib -I $(ROOTDIR)/stdlib -I $(ROOTDIR)/otherlibs/$(UNIXLIB)

OCAMLRUN?=$(ROOTDIR)/boot/ocamlrun
OCAMLC=$(OCAMLRUN) $(ROOTDIR)/ocamlc $(LIBS)
OCAMLOPT=$(OCAMLRUN) $(ROOTDIR)/ocamlopt $(LIBS)
OCAMLMKLIB=$(OCAMLRUN) $(ROOTDIR)/tools/ocamlmklib

COMPFLAGS=-w +33..39 -warn-error A -g -bin-annot -safe-string
ifeq "$(FLAMBDA)" "true"
OPTCOMPFLAGS=-O3
else
OPTCOMPFLAGS=
endif

MLFILES=thread.ml mutex.ml condition.ml event.ml
MLIFILES=thread.mli mutex.mli condition.mli event.mli

CMIFILES=$(MLIFILES:.mli=.cmi)
CMOFILES=$(MLFILES:.ml=.cmo)
CMXFILES=$(MLFILES:.ml=.cmx)

ifeq "$(UNIX_OR_WIN32)" "unix"
HEADER = externals-posix.h
else # Windows
HEADER = externals-win32.h
endif

# Note: the header on which object files produced from externals.c
# should actually depend is known for sure only at compile-time.
# That's why this dependency is handled in the Makefile directly
# and removed from the output of the C compiler during make depend

BYTECODE_C_OBJS=externals-bytecode.$(O)
NATIVECODE_C_OBJS=externals-native.$(O)

all: lib$(LIBNAME).$(A) $(LIBNAME).cma $(CMIFILES)

allopt: lib$(LIBNAME)nat.$(A) $(LIBNAME).cmxa $(CMIFILES)

lib$(LIBNAME).$(A): $(BYTECODE_C_OBJS)
	$(OCAMLMKLIB) -o $(LIBNAME) $(BYTECODE_C_OBJS) $(PTHREAD_LINK)

lib$(LIBNAME)nat.$(A): $(NATIVECODE_C_OBJS)
	$(OCAMLMKLIB) -o $(LIBNAME)nat $^

$(LIBNAME).cma: $(CMOFILES)
ifeq "$(UNIX_OR_WIN32)" "unix"
	$(OCAMLMKLIB) -o $(LIBNAME) -ocamlc '$(OCAMLC)' -cclib -lunix -linkall $(PTHREAD_CAML_LINK) $^
# TODO: Figure out why -cclib -lunix is used here.
# It may be because of the threadsUnix module which is deprecated.
# It may hence be good to figure out whether this module shouldn't be
# removed, and then -cclib -lunix arguments.
else # Windows
	$(OCAMLMKLIB) -o $(LIBNAME) -ocamlc "$(OCAMLC)" -linkall $(PTHREAD_CAML_LINK) $^
endif

# See remark above: force static linking of lib$(LIBNAME)nat.a
$(LIBNAME).cmxa: $(CMXFILES)
	$(OCAMLOPT) -linkall -a -cclib -l$(LIBNAME)nat $(PTHREAD_CAML_LINK) -o $@ $^

# Note: I removed "-cclib -lunix" from the line above.
# Indeed, if we link threads.cmxa, then we must also link unix.cmxa,
# which itself will pass -lunix to the C linker.  It seems more
# modular to me this way. -- Alain

# The following lines produce two object files externals-bytecode.$(O) and
# externals-native.$(O) from the same source file externals.c (it is compiled
# twice, each time with different options).

externals-bytecode.$(O): externals.c $(HEADER)
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -I$(ROOTDIR)/byterun  \
	  $(SHAREDCCCOMPOPTS) $(OUTPUTOBJ)$@ $<

externals-native.$(O): externals.c $(HEADER)
	$(CC) $(CFLAGS) $(CPPFLAGS) -I$(ROOTDIR)/asmrun -I$(ROOTDIR)/byterun \
	  $(SHAREDCCCOMPOPTS) -DNATIVE_CODE \
	  -DTARGET_$(ARCH) -DMODEL_$(MODEL) -DSYS_$(SYSTEM) \
	  $(OUTPUTOBJ)$@ -c $<

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f dll$(LIBNAME)*$(EXT_DLL) *.$(A) *.$(O)

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)
THREADS_LIBDIR=$(INSTALL_LIBDIR)/$(LIBNAME)
INSTALL_STUBLIBDIR=$(DESTDIR)$(STUBLIBDIR)

install:
	if test -f dll$(LIBNAME)$(EXT_DLL); then \
	  cp dll$(LIBNAME)$(EXT_DLL) "$(INSTALL_STUBLIBDIR)/dll$(LIBNAME)$(EXT_DLL)"; fi
	cp lib$(LIBNAME).$(A) "$(INSTALL_LIBDIR)"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) lib$(LIBNAME).$(A)
	mkdir -p "$(THREADS_LIBDIR)"
	cp $(CMIFILES) $(CMIFILES:.cmi=.cmti) $(LIBNAME).cma "$(THREADS_LIBDIR)"
	cp $(MLIFILES) "$(INSTALL_LIBDIR)"
	cp threads.h "$(INSTALL_LIBDIR)/caml"

installopt:
	cp lib$(LIBNAME)nat.$(A) "$(INSTALL_LIBDIR)"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) lib$(LIBNAME)nat.$(A)
	cp $(CMXFILES) $(LIBNAME).cmxa $(LIBNAME).$(A) "$(THREADS_LIBDIR)"
	cd "$(THREADS_LIBDIR)" && $(RANLIB) $(LIBNAME).$(A)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(OCAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(OCAMLOPT) -c $(COMPFLAGS) $(OPTCOMPFLAGS) $<

.PHONY: depend
ifeq "$(TOOLCHAIN)" "msvc"
depend:
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
depend:
	$(CC) -MM $(CPPFLAGS) -I$(ROOTDIR)/byterun externals.c \
	  | sed -e 's/externals\.o/externals-bytecode.$$(O)/' \
	  -e 's/ st_\(posix\|win32\)\.h//g' > .depend
	$(CC) -MM $(CPPFLAGS) -I$(ROOTDIR)/asmrun -I$(ROOTDIR)/byterun \
	  -DNATIVE_CODE -DTARGET_$(ARCH) -DMODEL_$(MODEL) -DSYS_$(SYSTEM) \
	  externals.c | sed -e 's/externals\.o/externals-native.$$(O)/' \
	  -e 's/ st_\(posix\|win32\)\.h//g' >> .depend
	$(CAMLRUN) ../../tools/ocamldep -slash *.mli *.ml >> .depend
endif

include .depend
