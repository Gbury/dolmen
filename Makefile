# copyright (c) 2014, guillaume bury

COMP=dune
FLAGS=--profile release
BINDIR=_build/install/default/bin

all: dune

dune:
	dune build --profile release

bin: dune
	cp $(BINDIR)/dolmen ./

test: bin
	cd tests && ./run

doc:
	dune build @doc

clean:
	$(COMP) clean && rm -f $(NAME)

.PHONY: all dune bin test doc clean
