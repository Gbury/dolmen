# copyright (c) 2014, guillaume bury

COMP=dune
FLAGS=--profile release
BIN=main
NAME=dolmen
BINDIR=_build/install/default/bin

all:
	$(COMP) build $(FLAGS)
	cp $(BINDIR)/$(BIN)* $(NAME)

test: all
	cd ../tests && ./run

clean:
	$(COMP) clean && rm -f $(NAME)

.PHONY: clean doc all bench install uninstall reinstall bin test
