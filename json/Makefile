package=tyttp-json.ipkg
executable=tyttp-json
idris2=idris2
codegen=node

.PHONY: build clean repl install dev dev-build test-clean test-build test dev-test

build:
	bash -c 'time pack build $(package)'

clean:
	rm -rf build

repl:
	pack --with-ipkg $(package) --rlwrap repl

run: build
	bash -c 'time node build/exec/$(executable)'

install:
	$(idris2) --install $(package) --codegen $(codegen)

dev: clean
	find src/ -name *.idr | entr make run

dev-build: clean
	find src/ -name *.idr | entr make build

test-clean:
	make -C tests clean

test-build:
	make -C tests build

test: test-build
	make -C tests test

dev-test:
	find . -name *.idr | INTERACTIVE="" entr make test

