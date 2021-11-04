# tyttp

A type safe _experimental_ web framework for idris2 targeting node js.

# Features

- HTTP server
- Streaming from end-to-end

- Routing - based on Alternative monad
  - Match HTTP methods
  - Parse HTTP url
  - Support path parameters, own path syntax
- Flexible: all combinators can be replaced

- Example file server in Main
- Example text based echo implementation in test

- Primitive Idris2 support for HTTP clients of NodeJS

# Prerequisities

1. idris2 version 0.5.1-babf346a7 or later, see [installation guide](https://github.com/idris-lang/Idris2/blob/main/INSTALL.md)
1. node LTS 14.x, see [download page](https://nodejs.org/en/download/)

# Build

    make clean
    make build

# Dev

    make repl

or

    make dev

# Distribution

    make dist
