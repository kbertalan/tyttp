# tyttp

A type safe _experimental_ web framework for idris2 targeting node js.

# Features

- HTTP/HTTPS server, HTTP2 server
- Streaming from end-to-end
- Supports async execution via an Inigo style Promise module

- Routing - based on Alternative monad
  - Match HTTP methods
  - Parse HTTP url
  - Support path parameters, own path syntax
- Flexible: all combinators can be replaced

- Example file server in test
- Example text based echo implementation in test

- Primitive Idris2 support for HTTP clients of NodeJS

- Serving a 41MB binary file for 1000 request with 4 requester on local machine takes 22.6 seconds, mean response time 90ms, 95% - 159ms, 99% 206ms.

# Prerequisities

1. idris2 version 0.5.1-394613432, see [installation guide](https://github.com/idris-lang/Idris2/blob/main/INSTALL.md)
1. node LTS 16.x, see [download page](https://nodejs.org/en/download/)

# Consumers

Supporting multiple formats can introduce many dependencies, so their handling is encouraged to be packaged separately.

Known consumers:

- [json](https://github.com/kbertalan/tyttp-json) for supporting `application/json`

# Build

    make clean
    make build

# Dev

    make dev

# Distribution

    make dist
