# copyright (c) 2014, guillaume bury

COMP=dune
FLAGS=
BINDIR=_build/install/default/bin

all: dune

dune:
	dune build $(FLAGS)

bin: dune
	cp $(BINDIR)/dolmen ./

test: bin
	cd tests && ./run

doc:
	dune build $(FLAGS) @doc

clean:
	$(COMP) clean

.PHONY: all dune bin test doc clean
