# copyright (c) 2014, guillaume bury

LOG=build.log
COMP=dune
FLAGS=--profile release
DOC=dolmen.docdir/index.html
BIN=main
NAME=dolmen
BINDIR=_build/install/default/bin
LIBDIR=_build/install/default/lib

libnames=dolmenclasses \
dolmendimacs \
dolmenicnf \
dolmeninterface \
dolmenline \
dolmensmtlib \
dolmenstandard \
dolmentptp \
dolmenzf

all: lib bin

lib:
	$(COMP) build $(FLAGS)

doc:
	ocamlbuild -use-ocamlfind -yaccflag '--table' $(DOC)

bin:
	$(COMP) build $(FLAGS)
	mv $(BINDIR)/$(BIN).* $(BINDIR)/$(NAME)
	cp $(BINDIR)/$(NAME) $(NAME)

test: bin
	cd ../tests && ./run

clean:
	$(COMP) clean && rm -f $(NAME)

TO_INSTALL=META $(foreach var,$(libnames),$(addprefix $(LIBDIR)/$(var)/$(var), .cma .cmxa .cmxs .a) )

install: lib
	ocamlfind install $(NAME) $(TO_INSTALL)

uninstall:
	ocamlfind remove $(NAME)

reinstall: lib
	ocamlfind remove $(NAME) || true
	ocamlfind install $(NAME) $(TO_INSTALL)

.PHONY: clean doc all bench install uninstall reinstall bin test
