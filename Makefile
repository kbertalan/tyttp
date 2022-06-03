package=tyttp.ipkg
executable=tyttp
idris2=idris2
codegen=node

.PHONY: build dist clean repl

KEY_FILE=./certs/key.pem
CERT_FILE=./certs/cert.pem

export KEY_FILE CERT_FILE

build:
	bash -c 'time $(idris2) --build $(package) --codegen $(codegen)'

dist: build
	npx rollup --config rollup.config.js

clean:
	rm -rf build
	rm -rf certs

repl:
	rlwrap $(idris2) --repl $(package) --codegen $(codegen)

$(KEY_FILE):
	mkdir ./certs
	openssl genrsa 2048 > ./certs/ca-key.pem
	openssl req -new -x509 -nodes -days 365000 -key ./certs/ca-key.pem -out ./certs/ca-cert.pem
	openssl req -newkey rsa:2048 -nodes -days 365000 -keyout $(KEY_FILE) -out ./certs/server-req.pem

$(CERT_FILE): $(KEY_FILE)
	openssl x509 -req -days 365000 -set_serial 01 -in ./certs/server-req.pem -out $(CERT_FILE) -CA ./certs/ca-cert.pem -CAkey ./certs/ca-key.pem

run: build $(KEY_FILE) $(CERT_FILE)
	bash -c 'time node build/exec/$(executable)'

install:
	$(idris2) --install $(package) --codegen $(codegen)

test-clean:
	make -C tests clean

test-build:
	make -C tests build

test: test-build
	make -C tests test

dev:
	find src/ -name *.idr | entr make run

dev-build:
	find src/ -name *.idr | entr make build

dev-test:
	find tests/ -name *.idr | INTERACTIVE="" entr make test

