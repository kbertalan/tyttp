module Node.HTTP.Get

import Node.HTTP

namespace Client
  
  export
  data ClientRequest : Type where [external]

  export
  data IncomingMessage : Type where [external]

  namespace Response

    export
    %foreign "node:lambda: res => res.headers"
    (.headers) : Client.IncomingMessage -> Headers

    export
    %foreign "node:lambda: res => res.statusCode"
    (.statusCode) : Client.IncomingMessage -> String

    %foreign "node:lambda: (ty, res, data) => res.on('data', a => data(a)())"
    ffi_onData : Client.IncomingMessage -> (a -> PrimIO ()) -> PrimIO ()

    export
    onData : Client.IncomingMessage -> (a -> IO ()) -> IO ()
    onData res cb = primIO $ ffi_onData res $ \a => toPrim $ cb a

    %foreign "node:lambda: (ty, res, error) => res.on('error', e => error(e)())"
    ffi_onError : Client.IncomingMessage -> (e -> PrimIO ()) -> PrimIO ()

    export
    onError : Client.IncomingMessage -> (a -> IO ()) -> IO ()
    onError res cb = primIO $ ffi_onError res $ \e => toPrim $ cb e

    %foreign "node:lambda: (ty, res, end) => res.on('end', end)"
    ffi_onEnd : Client.IncomingMessage -> PrimIO () -> PrimIO ()

    export
    onEnd : Client.IncomingMessage -> (() -> IO ()) -> IO ()
    onEnd res cb = primIO $ ffi_onEnd res $ toPrim $ cb ()

  namespace Request

    %foreign "node:lambda: req => req.end()"
    ffi_end : ClientRequest -> PrimIO ()

    export
    (.end) : ClientRequest -> IO ()
    (.end) res = primIO $ ffi_end res

    %foreign "node:lambda: (ty, req, data) => req.write(data)"
    ffi_write : { 0 a : _ } -> ClientRequest -> a -> PrimIO ()

    export
    (.write) : ClientRequest -> a -> IO ()
    (.write) res a = primIO $ ffi_write res a

%foreign "node:lambda: (http, url, cb) => http.get(url, (res) => { cb(res)() })"
ffi_get : HTTP -> String -> (Client.IncomingMessage -> PrimIO ()) -> PrimIO ClientRequest

export
(.get) : HTTP -> String -> (Client.IncomingMessage -> IO ()) -> IO ClientRequest
(.get) http url cb = primIO $ ffi_get http url $ \res => toPrim $ cb res

%foreign "node:lambda: (http, url, cb) => http.request(url, {method: 'POST'}, (res) => { cb(res)() })"
ffi_post : HTTP -> String -> (Client.IncomingMessage -> PrimIO ()) -> PrimIO ClientRequest

export
(.post) : HTTP -> String -> (Client.IncomingMessage -> IO ()) -> IO ClientRequest
(.post) http url cb = primIO $ ffi_post http url $ \res => toPrim $ cb res

