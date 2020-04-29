# copyright (c) 2014, guillaume bury

COMP=dune
FLAGS=
BINDIR=_build/install/default/bin

all: dune

watch:
	dune build $(FLAGS) -w @check

dune:
	dune build $(FLAGS) @install

doc:
	dune build $(FLAGS) @doc

test:
	dune exec -- tests/gentests.exe tests/
	dune build $(FLAGS) @runtest

clean:
	$(COMP) clean

.PHONY: all watch dune bin test doc clean
