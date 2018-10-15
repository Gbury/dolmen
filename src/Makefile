# copyright (c) 2014, guillaume bury

LOG=build.log
COMP=ocamlbuild
FLAGS=-use-ocamlfind -yaccflag '--table'
DOC=dolmen.docdir/index.html
BIN=main.native
NAME=dolmen

LIB=$(addprefix $(NAME), .cma .cmxa .cmxs .a)

all: lib bin

lib:
	$(COMP) $(FLAGS) $(LIB)

doc:
	$(COMP) $(FLAGS) $(DOC)

bin:
	$(COMP) $(FLAGS) $(BIN)
	cp $(BIN) $(NAME) && rm $(BIN)

test: bin
	cd ../tests && ./run

clean:
	$(COMP) -clean && rm -f $(NAME)

TO_INSTALL_LIB=$(addsuffix .a, $(NAME)) $(addsuffix .cmi, $(NAME)) $(addsuffix .cmx, $(NAME))
TO_INSTALL=META $(addprefix _build/,$(LIB) $(TO_INSTALL_LIB))

install: lib
	ocamlfind install $(NAME) $(TO_INSTALL)

uninstall:
	ocamlfind remove $(NAME)

reinstall: lib
	ocamlfind remove $(NAME) || true
	ocamlfind install $(NAME) $(TO_INSTALL)

.PHONY: clean doc all bench install uninstall reinstall bin test
