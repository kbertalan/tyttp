module Node.HTTP2.Server

import public Node.Error
import public Node.HTTP2
import public Node.Headers

export
data ServerHttp2Stream : Type where [external]

namespace Stream

  %foreign "node:lambda: (ty, stream, data) => { stream.on('data', a => data(a)()) }"
  ffi_onData : ServerHttp2Stream -> (a -> PrimIO ()) -> PrimIO ()

  export
  (.onData) : HasIO io => ServerHttp2Stream -> (a -> IO ()) -> io ()
  (.onData) stream cb = primIO $ ffi_onData stream $ \a => toPrim $ cb a

  %foreign "node:lambda: (stream, end) => { stream.on('end', () => end()()) }"
  ffi_onEnd : ServerHttp2Stream -> (() -> PrimIO ()) -> PrimIO ()

  export
  (.onEnd) : HasIO io => ServerHttp2Stream -> (() -> IO ()) -> io ()
  (.onEnd) stream cb = primIO $ ffi_onEnd stream $ \_ => toPrim $ cb ()

  %foreign "node:lambda: (ty, stream, error) => { stream.on('error', e => error(e)()) }"
  ffi_onError : ServerHttp2Stream -> (e -> PrimIO ()) -> PrimIO ()

  export
  (.onError) : HasIO io => ServerHttp2Stream -> (e -> IO ()) -> io ()
  (.onError) stream cb = primIO $ ffi_onError stream $ \e => toPrim $ cb e

  %foreign "node:lambda: stream => stream.end()"
  ffi_end : ServerHttp2Stream -> PrimIO ()

  export
  (.end) : HasIO io => ServerHttp2Stream -> io ()
  (.end) stream = primIO $ ffi_end stream

  %foreign "node:lambda: (ty, stream, data) => stream.write(data)"
  ffi_write : { 0 a : _ } -> ServerHttp2Stream -> a -> PrimIO ()

  export
  (.write) : HasIO io => ServerHttp2Stream -> a -> io ()
  (.write) stream a = primIO $ ffi_write stream a

  %foreign "node:lambda: (stream, headers) => stream.respond(headers)"
  ffi_respond : ServerHttp2Stream -> Headers -> PrimIO ()

  export
  (.respond) : HasIO io => ServerHttp2Stream -> Headers -> io ()
  (.respond) stream headers = primIO $ ffi_respond stream headers

  %foreign "node:lambda: (stream) => stream.pushAllowed ? 1 : 0"
  ffi_pushAllowed : ServerHttp2Stream -> Int

  export
  (.pushAllowed) : ServerHttp2Stream -> Bool
  (.pushAllowed) stream = 0 /= ffi_pushAllowed stream

  %foreign """
    node:lambda:
    (stream, headers, callback) => stream.pushStream(headers, (err, str, hs) => callback(err)(str)(hs)())
    """
  ffi_pushStream : ServerHttp2Stream -> Headers -> (NodeError -> ServerHttp2Stream -> Headers -> PrimIO ()) -> PrimIO ()

  export
  (.pushStream) : HasIO io => ServerHttp2Stream -> Headers -> (NodeError -> ServerHttp2Stream -> Headers -> IO ()) -> io ()
  (.pushStream) stream headers callback = primIO $ ffi_pushStream stream headers $ \err, str, hs => toPrim $ callback err str hs

export
data Http2Server : Type where [external]

%foreign "node:lambda: http2 => http2.createServer()"
ffi_createServer : HTTP2 -> PrimIO Http2Server

export
(.createServer) : HasIO io => HTTP2 -> io Http2Server
(.createServer) http2 = primIO $ ffi_createServer http2

%foreign """
  node:lambda:
  (http2, key, cert) =>
    http2.createSecureServer({
      key, cert
    })
  """
ffi_createSecureServer : HTTP2 -> String -> String -> PrimIO Http2Server

export
(.createSecureServer) : HasIO io => HTTP2 -> String -> String -> io Http2Server
(.createSecureServer) http2 key cert = primIO $ ffi_createSecureServer http2 key cert

%foreign "node:lambda: (server, handler) => server.on('stream', (stream, headers) => handler(stream)(headers)())"
ffi_onStream : Http2Server -> (ServerHttp2Stream -> Headers -> PrimIO ()) -> PrimIO ()

export
(.onStream) : HasIO io => Http2Server -> (ServerHttp2Stream -> Headers -> IO()) -> io ()
(.onStream) server callback = 
  let primCallback = \stream => \headers => toPrim $ callback stream headers
  in primIO $ ffi_onStream server primCallback

%foreign "node:lambda: (server, port) => server.listen(port)"
ffi_listen : Http2Server -> Int -> PrimIO ()

export
(.listen) : HasIO io => Http2Server -> Int -> io ()
(.listen) server port = primIO $ ffi_listen server port

%foreign "node:lambda: server => server.close()"
ffi_close : Http2Server -> PrimIO ()

export
(.close) : HasIO io => Http2Server -> io ()
(.close) server = primIO $ ffi_close server

