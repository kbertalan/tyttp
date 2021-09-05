package=tyttp.ipkg
executable=tyttp
idris2=idris2
codegen=node

.PHONY: build dist clean repl

build:
	$(idris2) --build $(package) --codegen $(codegen)

dist: build
	npx rollup --config rollup.config.js

clean:
	rm -rf build

repl:
	rlwrap $(idris2) --repl $(package)

run: build
	bash -c 'time node build/exec/$(executable)'

install:
	$(idris2) --install $(package)

test-clean:
	make -C tests clean

test-build:
	make -C tests build

test: test-build
	make -C tests test

