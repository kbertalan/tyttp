module Node.HTTP.Client

import public Node.HTTP

export
data ClientRequest : Type where [external]

export
data IncomingMessage : Type where [external]

namespace Request

  %foreign "node:lambda: req => req.end()"
  ffi_end : ClientRequest -> PrimIO ()

  export
  (.end) : HasIO io => ClientRequest -> io ()
  (.end) res = primIO $ ffi_end res

  %foreign "node:lambda: (ty, req, data) => req.write(data)"
  ffi_write : { 0 a : _ } -> ClientRequest -> a -> PrimIO ()

  export
  (.write) : HasIO io => ClientRequest -> a -> io ()
  (.write) res a = primIO $ ffi_write res a

namespace Response

  export
  %foreign "node:lambda: res => res.headers"
  (.headers) : IncomingMessage -> Headers

  export
  %foreign "node:lambda: res => res.statusCode"
  (.statusCode) : IncomingMessage -> String

  %foreign "node:lambda: (ty, res, data) => res.on('data', a => data(a)())"
  ffi_onData : IncomingMessage -> (a -> PrimIO ()) -> PrimIO ()

  export
  onData : HasIO io => IncomingMessage -> (a -> IO ()) -> io ()
  onData res cb = primIO $ ffi_onData res $ \a => toPrim $ cb a

  %foreign "node:lambda: (ty, res, error) => res.on('error', e => error(e)())"
  ffi_onError : IncomingMessage -> (e -> PrimIO ()) -> PrimIO ()

  export
  onError : HasIO io => IncomingMessage -> (a -> IO ()) -> io ()
  onError res cb = primIO $ ffi_onError res $ \e => toPrim $ cb e

  %foreign "node:lambda: (ty, res, end) => res.on('end', end)"
  ffi_onEnd : IncomingMessage -> PrimIO () -> PrimIO ()

  export
  onEnd : HasIO io => IncomingMessage -> (() -> IO ()) -> io ()
  onEnd res cb = primIO $ ffi_onEnd res $ toPrim $ cb ()

%foreign "node:lambda: (http, url, cb) => http.get(url, (res) => { cb(res)() })"
ffi_get : HTTP -> String -> (IncomingMessage -> PrimIO ()) -> PrimIO ClientRequest

export
(.get) : HasIO io => HTTP -> String -> (IncomingMessage -> IO ()) -> io ClientRequest
(.get) http url cb = primIO $ ffi_get http url $ \res => toPrim $ cb res

%foreign "node:lambda: (http, url, cb) => http.request(url, {method: 'POST'}, (res) => { cb(res)() })"
ffi_post : HTTP -> String -> (IncomingMessage -> PrimIO ()) -> PrimIO ClientRequest

export
(.post) : HasIO io => HTTP -> String -> (IncomingMessage -> IO ()) -> io ClientRequest
(.post) http url cb = primIO $ ffi_post http url $ \res => toPrim $ cb res

