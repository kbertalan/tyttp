package=tyttp.ipkg
executable=tyttp
idris2=idris2
codegen=node

.PHONY: build dist clean repl run install test-clean test-build test dev dev-build dev-test

KEY_FILE=./certs/key.pem
CERT_FILE=./certs/cert.pem
CA_FILE=./certs/ca-cert.pem

export KEY_FILE CERT_FILE

build:
	bash -c 'time pack build $(package)'

dist: build
	npx rollup --config rollup.config.js

clean:
	rm -rf build
	rm -rf certs

repl:
	pack --with-ipkg $(package) --rlwrap repl

certs:
	mkdir ./certs

$(CA_FILE): certs
	openssl genrsa 2048 > ./certs/ca-key.pem
	openssl req -new -x509 -nodes -days 365000 -key ./certs/ca-key.pem -out $(CA_FILE)

$(KEY_FILE): $(CA_FILE)
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

test: install test-build
	make -C tests test

dev:
	find src/ -name *.idr | entr make run

dev-build:
	find src/ -name *.idr | entr make build

dev-test:
	find . -name *.idr | INTERACTIVE="" entr make test

