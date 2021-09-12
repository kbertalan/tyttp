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

  %foreign "node:lambda: (ty, a) => JSON.stringify(a, null, 2)"
  ffi_toJsonString : a -> String

  export
  toJsonString : a -> String
  toJsonString a = ffi_toJsonString a


  %foreign "node:lambda: (ty, a) => console.log(a)"
  ffi_debugJsValue : a -> PrimIO ()

  export
  debugJsValue : a -> IO ()
  debugJsValue a = primIO $ ffi_debugJsValue a

  namespace Response

    %foreign "node:lambda: res => res.end()"
    ffi_end : ServerResponse -> PrimIO ()

    export
    (.end) : ServerResponse -> IO ()
    (.end) res = primIO $ ffi_end res
