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

    pack build tyttp.ipkg
    pack build adapter-node/tyttp-adapter-node.ipkg

# Test

    pack test tyttp
    pack test tyttp-adapter-node

# Run a sample from tyttp-adapter-node

    pack run tyttp-adapter-node

# Run all builds and tests via docker

    docker build . -t tyttp:latest
    docker run --rm -it -p 3000:3000 tyttp:latest

Then you can access the running application on port 3000:

    curl http://localhost:3000/query?query-string=will-be-sent-back
