# copyright (c) 2014, guillaume bury

COMP=dune
FLAGS=
BINDIR=_build/install/default/bin

all: dune

watch:
	dune build $(FLAGS) -w

dune:
	dune build $(FLAGS)

doc:
	dune build $(FLAGS) @doc

clean:
	$(COMP) clean

.PHONY: all watch dune bin test doc clean
