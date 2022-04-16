module Node.HTTP2.Client

import public Node.HTTP2
import public Node.Headers

export
data ClientHttp2Session : Type where [external]

export
data ClientHttp2Stream : Type where [external]

namespace Stream

  %foreign "node:lambda: stream => stream.end()"
  ffi_end : ClientHttp2Stream -> PrimIO ()

  export
  (.end) : HasIO io => ClientHttp2Stream -> io ()
  (.end) stream = primIO $ ffi_end stream

  %foreign "node:lambda: (ty, stream, data) => stream.write(data)"
  ffi_write : { 0 a : _ } -> ClientHttp2Stream -> a -> PrimIO ()

  export
  (.write) : HasIO io => ClientHttp2Stream -> a -> io ()
  (.write) stream a = primIO $ ffi_write stream a

  %foreign "node:lambda: (ty, stream, data) => stream.on('data', a => data(a)())"
  ffi_onData : ClientHttp2Stream -> (a -> PrimIO ()) -> PrimIO ()

  export
  onData : HasIO io => ClientHttp2Stream -> (a -> IO ()) -> io ()
  onData stream cb = primIO $ ffi_onData stream $ \a => toPrim $ cb a

  %foreign "node:lambda: (ty, stream, error) => stream.on('error', e => error(e)())"
  ffi_onError : ClientHttp2Stream -> (e -> PrimIO ()) -> PrimIO ()

  export
  onError : HasIO io => ClientHttp2Stream -> (a -> IO ()) -> io ()
  onError stream cb = primIO $ ffi_onError stream $ \e => toPrim $ cb e

  %foreign "node:lambda: (stream, end) => stream.on('end', end)"
  ffi_onEnd : ClientHttp2Stream -> PrimIO () -> PrimIO ()

  export
  onEnd : HasIO io => ClientHttp2Stream -> IO () -> io ()
  onEnd stream cb = primIO $ ffi_onEnd stream $ toPrim $ cb

  %foreign "node:lambda: (stream, response) => stream.on('response', headers => response(headers)())"
  ffi_onResponse : ClientHttp2Stream -> (Headers -> PrimIO ()) -> PrimIO ()

  export
  (.onResponse) : HasIO io => ClientHttp2Stream -> (Headers -> IO ()) -> io ()
  (.onResponse) stream cb = primIO $ ffi_onResponse stream $ \h => toPrim $ cb h

  %foreign "node:lambda: (stream, handler) => stream.on('push', headers => handler(headers)())"
  ffi_onPush : ClientHttp2Stream -> (Headers -> PrimIO ()) -> PrimIO ()

  export
  (.onPush) : HasIO io => ClientHttp2Stream -> (Headers -> IO ()) -> io ()
  (.onPush) stream cb = primIO $ ffi_onPush stream $ \h => toPrim $ cb h

namespace Session

  %foreign """
    node:lambda:
    (session, method, path, headers) =>
      session.request({
        ...headers,
        ':method': method,
        ':path': path
      })
    """
  ffi_request : ClientHttp2Session -> String -> String -> Headers -> PrimIO ClientHttp2Stream

  export
  (.get) : HasIO io => ClientHttp2Session -> String -> Headers -> io ClientHttp2Stream
  (.get) session path headers = primIO $ ffi_request session "GET" path headers

  export
  (.post) : HasIO io => ClientHttp2Session -> String -> Headers -> io ClientHttp2Stream
  (.post) session path headers = primIO $ ffi_request session "POST" path headers

  %foreign "node:lambda: (session) => session.close()"
  ffi_close : ClientHttp2Session -> PrimIO ()

  export
  (.close) : HasIO io => ClientHttp2Session -> io ()
  (.close) session = primIO $ ffi_close session

  %foreign """
    node:lambda:
    (session, handler) =>
      session.on('stream', (pushedStream, requestHeaders) => handler(pushedStream)(requestHeaders)())
    """
  ffi_onStream : ClientHttp2Session -> (ClientHttp2Stream -> Headers -> PrimIO ()) -> PrimIO ()

  export
  (.onStream) : HasIO io => ClientHttp2Session -> (ClientHttp2Stream -> Headers -> IO ()) -> io ()
  (.onStream) session handler = primIO $ ffi_onStream session $ \stream, headers => toPrim $ handler stream headers

%foreign "node:lambda: (http2, authority) => http2.connect(authority)"
ffi_connect : HTTP2 -> String -> PrimIO ClientHttp2Session

export
(.connect) : HTTP2 -> String -> IO ClientHttp2Session
(.connect) http2 authority = primIO $ ffi_connect http2 authority

