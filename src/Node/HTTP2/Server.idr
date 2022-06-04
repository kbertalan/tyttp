module Node.HTTP2.Server

import Data.Maybe
import public Node.Error
import public Node.HTTP2
import public Node.Headers
import public Node.Net.Server

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

public export
data PaddingStrategy
  = None
  | Max
  | Aligned

export
data NodePaddingStrategy : Type where [external]

%foreign "node:lambda: (http2) => http2.constants.PADDING_STRATEGY_NONE"
ffi_paddingStrategy_None : {auto http2 : HTTP2} -> NodePaddingStrategy

%foreign "node:lambda: (http2) => http2.constants.PADDING_STRATEGY_MAX"
ffi_paddingStrategy_Max : {auto http2 : HTTP2} -> NodePaddingStrategy

%foreign "node:lambda: (http2) => http2.constants.PADDING_STRATEGY_ALIGNED"
ffi_paddingStrategy_Aligned : {auto http2 : HTTP2} -> NodePaddingStrategy

export
convertPaddingStrategy : {auto http2 : HTTP2} -> PaddingStrategy -> NodePaddingStrategy
convertPaddingStrategy = \case
  None => ffi_paddingStrategy_None
  Max => ffi_paddingStrategy_Max
  Aligned => ffi_paddingStrategy_Aligned

public export
record Settings where
  constructor MkSettings
  headerTableSize: Int
  enablePush: Bool
  initialWindowSize: Int
  maxFrameSize: Int
  maxConcurrentStreams: Double
  maxHeaderListSize: Int
  enableConnectProtocol: Bool

export
defaultSettings : Settings
defaultSettings = MkSettings
  { headerTableSize = 4096
  , enablePush = True
  , initialWindowSize = 65535
  , maxFrameSize = 16384
  , maxConcurrentStreams = 4294967295.0
  , maxHeaderListSize = 65535
  , enableConnectProtocol = False
  }

export
data NodeHTTP2Settings : Type where [external]

%foreign """
  node:lambda:
  ( headerTableSize
  , enablePush
  , initialWindowSize
  , maxFrameSize
  , maxConcurrentStreams
  , maxHeaderListSize
  , enableConnectProtocol
  ) => ({
    headerTableSize,
    enablePush: !!enablePush && undefined, // -- TODO check why value true not accepted?
    initialWindowSize,
    maxFrameSize,
    maxConcurrentStreams,
    maxHeaderListSize,
    enableConnectProtocol: enableConnectProtocol != 0
  })
  """
ffi_convertSettings :
  (headerTableSize: Int) ->
  (enablePush: Int) ->
  (initialWindowSize: Int) ->
  (maxFrameSize: Int) ->
  (maxConcurrentStreams: Double) ->
  (maxHeaderListSize: Int) ->
  (enableConnectProtocol: Int) ->
  NodeHTTP2Settings

export
convertSettings : Settings -> NodeHTTP2Settings
convertSettings s = ffi_convertSettings
  s.headerTableSize
  (if s.enablePush then 1 else 0)
  s.initialWindowSize
  s.maxFrameSize
  s.maxConcurrentStreams
  s.maxHeaderListSize
  (if s.enableConnectProtocol then 1 else 0)

public export
record Options where
  constructor MkOptions
  maxDeflateDynamicTableSize: Int
  maxSettings: Int
  maxSessionMemory: Int
  maxHeaderListPairs: Int
  maxOutstandingPings: Int
  maxSendHeaderBlockLength: Maybe Int
  paddingStrategy: PaddingStrategy
  peerMaxConcurrentStreams: Int
  maxSessionInvalidFrames: Int 
  maxSessionRejectedStreams: Int
  settings: Settings
  unknownProtocolTimeout: Int

export
defaultOptions : HTTP2.Server.Options
defaultOptions = MkOptions
  { maxDeflateDynamicTableSize = 4096
  , maxSettings = 32
  , maxSessionMemory = 10
  , maxHeaderListPairs = 128
  , maxOutstandingPings = 10
  , maxSendHeaderBlockLength = Nothing
  , paddingStrategy = None
  , peerMaxConcurrentStreams = 100
  , maxSessionInvalidFrames = 1000
  , maxSessionRejectedStreams = 100
  , settings = defaultSettings
  , unknownProtocolTimeout = 10000
  }

export
data NodeHTTP2ServerOptions : Type where [external]

%foreign """
  node:lambda:
  ( maxDeflateDynamicTableSize
  , maxSettings
  , maxSessionMemory
  , maxHeaderListPairs
  , maxOutstandingPings
  , maxSendHeaderBlockLength
  , paddingStrategy
  , peerMaxConcurrentStreams
  , maxSessionInvalidFrames
  , maxSessionRejectedStreams
  , settings
  , unknownProtocolTimeout
  ) => ({
    maxDeflateDynamicTableSize,
    maxSettings,
    maxSessionMemory,
    maxHeaderListPairs,
    maxOutstandingPings,
    maxSendHeaderBlockLength: maxSendHeaderBlockLength != -1 ? maxSendHeaderBlockLength : undefined,
    paddingStrategy,
    peerMaxConcurrentStreams,
    maxSessionInvalidFrames,
    maxSessionRejectedStreams,
    settings,
    unknownProtocolTimeout
  })
  """
ffi_convertOptions :
  (maxDeflateDynamicTableSize: Int) ->
  (maxSettings: Int) ->
  (maxSessionMemory: Int) ->
  (maxHeaderListPairs: Int) ->
  (maxOutstandingPings: Int) ->
  (maxSendHeaderBlockLength: Int) ->
  (paddingStrategy: NodePaddingStrategy) ->
  (peerMaxConcurrentStreams: Int) ->
  (maxSessionInvalidFrames: Int ) ->
  (maxSessionRejectedStreams: Int) ->
  (settings: NodeHTTP2Settings) ->
  (unknownProtocolTimeout: Int) ->
  NodeHTTP2ServerOptions

export
convertOptions : {auto http2 : HTTP2} -> HTTP2.Server.Options -> NodeHTTP2ServerOptions
convertOptions o = ffi_convertOptions
  o.maxDeflateDynamicTableSize
  o.maxSettings
  o.maxSessionMemory
  o.maxHeaderListPairs
  o.maxOutstandingPings
  (fromMaybe (-1) o.maxSendHeaderBlockLength)
  (convertPaddingStrategy o.paddingStrategy)
  o.peerMaxConcurrentStreams
  o.maxSessionInvalidFrames
  o.maxSessionRejectedStreams
  (convertSettings o.settings)
  o.unknownProtocolTimeout

export
data Http2Server : Type where [external]

%foreign "node:lambda: (http2, netServerOptions, serverOptions) => http2.createServer({...netServerOptions, ...serverOptions})"
ffi_createServer : HTTP2 -> NodeServerOptions -> NodeHTTP2ServerOptions -> PrimIO Http2Server

export
(.createServer) : HasIO io => HTTP2 -> Net.Server.Options -> HTTP2.Server.Options -> io Http2Server
(.createServer) http2 netServerOptions serverOptions = primIO $ ffi_createServer http2 (convertOptions netServerOptions) (convertOptions serverOptions)

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

%foreign "node:lambda: (server, options) => server.listen(options)"
ffi_listen : Http2Server -> NodeListenOptions -> PrimIO ()

export
(.listen) : HasIO io => Http2Server -> Listen.Options -> io ()
(.listen) server options = primIO $ ffi_listen server $ convertOptions options

%foreign "node:lambda: server => server.close()"
ffi_close : Http2Server -> PrimIO ()

export
(.close) : HasIO io => Http2Server -> io ()
(.close) server = primIO $ ffi_close server

