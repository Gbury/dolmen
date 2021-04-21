# copyright (c) 2014, guillaume bury

COMP=dune
FLAGS=
BINDIR=_build/install/default/bin

# Optional arguments for the tune executions
ifeq ($(J),)
J := 1
endif

all: build

watch:
	dune build $(FLAGS) -w @check

build:
	dune build $(FLAGS) @install

doc:
	dune build $(FLAGS) @doc

gentests: $(wildcard tests/**/*)
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

tune.config: build
	dune exec -- tools/tune/tune.exe create -o tune.config \
		--minor-heap-size=200_000,200_000,1_000_000 \
		--major-heap-increment=15,15,105 \
		--space-overhead=80,20,200 \
		--max-overhead=500,500,1000 \
		--allocation-policy=0,1,2 \
		-- _build/install/default/bin/dolmen --gc-env $(FILE) -s 2G -t 5m

tune.res: build tune.config
	dune exec -- tools/tune/tune.exe run -i tune.config -o tune.res -j $(J)

tune-graphs: build tune.res
	dune exec -- tools/tune/tune.exe graph -i tune.res

clean:
	find . -name '*.coverage' | xargs rm -f
	rm -rf _coverage tune.*
	$(COMP) clean

.PHONY: all watch dune doc gentests test promote coverage tune-graphs clean
