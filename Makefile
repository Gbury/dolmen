# copyright (c) 2014, guillaume bury

FLAGS?=
BINDIR=_build/install/default/bin

all: build

watch:
	dune build $(FLAGS) -w @check

build:
	dune build $(FLAGS) @install

top:
	dune utop

doc:
	dune build $(FLAGS) @doc

gentests: $(filter-out $(wildcard tests/**/*.t/**/*),$(wildcard tests/**/*))
	dune exec -- tools/gentests.exe tests/

test: gentests
	dune build $(FLAGS) @runtest

promote: gentests
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
	dune clean

.PHONY: all watch dune doc gentests test promote coverage clean
