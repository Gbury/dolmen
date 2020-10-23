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

test: dune
	dune exec -- tools/gentests.exe tests/
	dune build $(FLAGS) @runtest

test-promote: dune
	dune exec -- tools/gentests.exe tests/
	-dune build $(FLAGS) @runtest
	dune promote $(FLAGS)

coverage:
	$(MAKE) clean
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html
	@xdg-open _coverage/index.html
	bisect-ppx-report summary

clean:
	find . -name '*.coverage' | xargs rm -f
	rm -rf _coverage
	$(COMP) clean

.PHONY: all watch dune bin test doc clean
