module Node.HTTP

export
data HTTP : Type where [external]

%foreign "node:lambda: () => require('http')"
ffi_require : () -> PrimIO HTTP

export
require : IO HTTP
require = primIO $ ffi_require ()

export
data Server : Type where [external]

%foreign "node:lambda: http => http.createServer()"
ffi_createServer : HTTP -> PrimIO Server

export
(.createServer) : HTTP -> IO Server
(.createServer) http = primIO $ ffi_createServer http

namespace Headers

  export
  data Headers : Type where [external]

  %foreign "node:lambda: () => { return {}; }"
  ffi_empty : () -> Headers

  export
  empty : Headers
  empty = ffi_empty ()

  %foreign "node:lambda: (name, value) => { const headers = {}; headers[name] = value; return headers; }"
  ffi_singleton : String -> String -> Headers

  export
  singleton : String -> String -> Headers
  singleton name value = ffi_singleton name value

  %foreign "node:lambda: (headers, name, value) => { headers[name] = value; return headers; }"
  ffi_setHeader : Headers -> String -> String -> PrimIO Headers

  export
  (.setHeader) : Headers -> String -> String -> IO Headers
  (.setHeader) headers name value = primIO $ ffi_setHeader headers name value

namespace Server

  export
  data IncomingMessage : Type where [external]

  export
  data ServerResponse : Type where [external]

  %foreign "node:lambda: (server, handler) => server.on('request', (req, res) => handler(req)(res)())"
  ffi_onRequest : Server -> (IncomingMessage -> ServerResponse -> PrimIO ()) -> PrimIO ()

  export
  (.onRequest) : Server -> (IncomingMessage -> ServerResponse -> IO()) -> IO ()
  (.onRequest) server callback = 
    let primCallback = \req => \res => toPrim $ callback req res
    in primIO $ ffi_onRequest server primCallback

  %foreign "node:lambda: (server, port) => server.listen(port)"
  ffi_listen : Server -> Int -> PrimIO ()

  export
  (.listen) : Server -> Int -> IO ()
  (.listen) server port = primIO $ ffi_listen server port

  %foreign "node:lambda: server => server.close()"
  ffi_close : Server -> PrimIO ()

  export
  (.close) : Server -> IO ()
  (.close) server = primIO $ ffi_close server

  namespace Request

    export
    %foreign "node:lambda: req => req.headers"
    (.headers) : IncomingMessage -> Headers

    export
    %foreign "node:lambda: req => req.httpVersion"
    (.httpVersion) : IncomingMessage -> String

    export
    %foreign "node:lambda: req => req.method"
    (.method) : IncomingMessage -> String

    export
    %foreign "node:lambda: req => req.url"
    (.url) : IncomingMessage -> String

    %foreign "node:lambda: (ty, req, data) => { req.on('data', a => data(a)()) }"
    ffi_onData : IncomingMessage -> (a -> PrimIO ()) -> PrimIO ()

    export
    (.onData) : IncomingMessage -> (a -> IO ()) -> IO ()
    (.onData) req cb = primIO $ ffi_onData req $ \a => toPrim $ cb a

    %foreign "node:lambda: (req, end) => { req.on('end', () => end()()) }"
    ffi_onEnd : IncomingMessage -> (() -> PrimIO ()) -> PrimIO ()

    export
    (.onEnd) : IncomingMessage -> (() -> IO ()) -> IO ()
    (.onEnd) req cb = primIO $ ffi_onEnd req $ \_ => toPrim $ cb ()

    %foreign "node:lambda: (ty, req, error) => { req.on('error', e => error(e)()) }"
    ffi_onError : IncomingMessage -> (e -> PrimIO ()) -> PrimIO ()

    export
    (.onError) : IncomingMessage -> (e -> IO ()) -> IO ()
    (.onError) req cb = primIO $ ffi_onError req $ \e => toPrim $ cb e

  namespace Response

    %foreign "node:lambda: res => res.end()"
    ffi_end : ServerResponse -> PrimIO ()

    export
    (.end) : ServerResponse -> IO ()
    (.end) res = primIO $ ffi_end res

    %foreign "node:lambda: (ty, res, data) => res.write(data)"
    ffi_write : { 0 a : _ } -> ServerResponse -> a -> PrimIO ()

    export
    (.write) : ServerResponse -> a -> IO ()
    (.write) res a = primIO $ ffi_write res a

    %foreign "node:lambda: (res, status, headers) => res.writeHead(status, headers)"
    ffi_writeHead : ServerResponse -> Int -> Headers -> PrimIO ()

    export
    (.writeHead) : ServerResponse -> Int -> Headers -> IO ()
    (.writeHead) res status headers = primIO $ ffi_writeHead res status headers
