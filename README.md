# tyttp

A type safe _experimental_ web framework for idris2 targeting node js.

# Features

- HTTP/HTTPS server, HTTP2 server
- Streaming from end-to-end
- Supports async execution via [idris2-promise](https://github.com/kbertalan/idris2-promise)

- Routing - based on Alternative monad
  - Match HTTP methods
  - Parse HTTP url
  - Support path parameters, own path syntax
- Flexible: all combinators can be replaced

- Example file server in test
- Example text based echo implementation in test

# Prerequisities

1. [idris2-pack](https://github.com/stefan-hoeck/idris2-pack)
1. idris2 version 0.5.1, see installation at idris2-pack documentation
1. node LTS 16.x, see [download page](https://nodejs.org/en/download/)

# Consumers and Producers

Supporting multiple formats can introduce many dependencies, so their handling is encouraged to be packaged separately.

Known consumers and producers:

- [json](https://github.com/kbertalan/tyttp-json) for supporting `application/json`

# Build

    make clean
    make build

# Dev

    make dev

# Distribution

    make dist

