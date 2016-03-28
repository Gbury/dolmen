# copyright (c) 2014, guillaume bury

LOG=build.log
COMP=ocamlbuild
FLAGS=-use-menhir -use-ocamlfind
DIRS=-Is common,interface,languages/dimacs,languages/zf,languages/smtlib
DOC=dolmen.docdir/index.html
BIN=main.native
NAME=dolmen

LIB=$(addprefix $(NAME), .cma .cmxa .cmxs)

all: lib bin

lib:
	$(COMP) $(FLAGS) $(DIRS) $(LIB)

doc:
	$(COMP) $(FLAGS) $(DIRS) $(DOC)

bin:
	$(COMP) $(FLAGS) $(DIRS) $(BIN)
	cp $(BIN) $(NAME) && rm $(BIN)

test: bin
	cd ../tests && ./run

clean:
	$(COMP) -clean && rm -f $(NAME)

TO_INSTALL=META $(addprefix _build/,$(LIB) $(NAME).a $(NAME).cmi)

install: lib
	ocamlfind install $(NAME) $(TO_INSTALL)

uninstall:
	ocamlfind remove $(NAME)

reinstall: all
	ocamlfind remove $(NAME) || true
	ocamlfind install $(NAME) $(TO_INSTALL)

.PHONY: clean doc all bench install uninstall reinstall enable_log disable_log bin test
