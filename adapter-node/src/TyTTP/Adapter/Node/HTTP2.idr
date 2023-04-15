module TyTTP.Adapter.Node.HTTP2

import public Data.Buffer
import Data.Buffer.Ext
import Data.String
import Data.Maybe
import public Node.Error
import public Node.HTTP2
import Node.JS.Misc
import Node.JS.Std.JSON
import TyTTP
import TyTTP.URL
import public TyTTP.Adapter.Node.Error
import TyTTP.HTTP

namespace Fields

  public export
  data RequestPseudoHeaderField
    = Method
    | Scheme
    | Authority
    | Path

  public export
  Show RequestPseudoHeaderField where
    show f = case f of
      Method => ":method"
      Scheme => ":scheme"
      Authority => ":authority"
      Path => ":path"

  public export
  data ResponsePseudoHeaderField
    = Status

  public export
  Show ResponsePseudoHeaderField where
    show Status = ":status"

public export
RawHttpRequest : Type
RawHttpRequest = HttpRequest SimpleURL StringHeaders $ Publisher IO Error Buffer

public export
RawHttpResponse : Type
RawHttpResponse = Response Status StringHeaders $ Publisher IO Error Buffer

public export
PushContext : Type
PushContext = Context Method SimpleURL Version StringHeaders Status StringHeaders () $ Publisher IO Error Buffer

sendResponse : RawHttpResponse -> ServerHttp2Stream -> IO ()
sendResponse res stream = do
  let status = res.status.code
  headers <- mapHeaders res.headers status

  stream.respond headers
  res.body.subscribe $ MkSubscriber
        { onNext = \a => stream.write a Nothing }
        { onFailed = \e => pure () }
        { onSucceded = \_ => stream.end Nothing { d = Buffer } }
  where
    mapHeaders : StringHeaders -> Int -> IO Headers
    mapHeaders h s = do
      let newHeaders = singleton (show Fields.Status) (show s)
      foldlM (\hs, (k,v) => hs.setHeader k v) newHeaders h

sendResponseFromPromise : Error e
  => (String -> RawHttpResponse)
  -> Promise e IO (Context Method SimpleURL Version StringHeaders Status StringHeaders b $ Publisher IO Error Buffer)
  -> ServerHttp2Stream
  -> IO ()
sendResponseFromPromise errorHandler (MkPromise cont) stream =
  let callbacks = MkCallbacks
        { onSucceded = \a => sendResponse a.response stream }
        { onFailed = \e => sendResponse (errorHandler $ TyTTP.Core.Error.message e) stream }
  in
    cont callbacks

parseRequest : ServerHttp2Stream -> Headers -> Either String RawHttpRequest
parseRequest stream headers =
  let Just method = parseMethod <$> headers.getHeader (show Fields.Method)
        | Nothing => Left "Method header is missing from request"
      scheme = parse <$> headers.getHeader (show Fields.Scheme)
      authority = headers.getHeader (show Fields.Authority)
      Just pathAndSearch = headers.getHeader (show Fields.Path)
        | Nothing => Left "Path header is missing from request"
      (path, search) = String.break (=='?') pathAndSearch
      url = MkURL scheme authority path search
      version = Version_2
  in Right $ mkRequest method url version headers.asList $ MkPublisher $ \s => do
        stream.onData s.onNext
        (Readable.(.onError)) stream s.onFailed
        stream.onEnd $ s.onSucceded ()

pusher : HasIO io => ServerHttp2Stream -> Lazy PushContext -> io ()
pusher parent ctx = do
  reqHeaders <- mapHeaders $ ctx.request.headers
    <+> (maybe [] pure $ map ((show Fields.Scheme,) . show) ctx.request.url.scheme)
    <+> (maybe [] pure $ map (show Fields.Authority,) ctx.request.url.authority)
    <+> [ (show Fields.Method, show ctx.request.method)
        , (show Fields.Path, ctx.request.url.path)
        ]
  parent.pushStream reqHeaders $ \err, stream, headers => do
    if truthy err then putStrLn "ERROR: \{JSON.stringify err 2}"
                  else sendResponse ctx.response stream
    where
      mapHeaders : StringHeaders -> io Headers
      mapHeaders h = do
        newHeaders <- empty
        foldlM (\hs, (k,v) => hs.setHeader k v) newHeaders h

public export
record Options where
  constructor MkOptions
  netServerOptions : Net.CreateServer.Options
  serverOptions : HTTP2.CreateServer.Options
  listenOptions : Listen.Options
  errorHandler : String -> RawHttpResponse

export
defaultOptions : HTTP2.Options
defaultOptions = MkOptions
  { netServerOptions = Net.CreateServer.defaultOptions
  , serverOptions = HTTP2.CreateServer.defaultOptions
  , listenOptions =
    { port := Just 3000
    , host := Just "localhost"
    } Listen.defaultOptions
  , errorHandler = \e => MkResponse
    { status = INTERNAL_SERVER_ERROR
    , headers =
      [ ("Content-Type", "text/plain")
      , ("Content-Length", show $ length e)
      ]
    , body = singleton $ fromString e
    }
  }

export
listen : HasIO io
   => HasIO pushIO
   => Error e
   => HTTP2Module
   -> HTTP2.Options
   -> (
        (Lazy PushContext -> pushIO ())
        -> Context Method SimpleURL Version StringHeaders Status StringHeaders (Publisher IO Error Buffer) ()
        -> Promise e IO $ Context Method SimpleURL Version StringHeaders Status StringHeaders b (Publisher IO Error Buffer)
      )
   -> io Http2Server
listen http2 options handler = do
  server <- http2.createServer $ MkOptions
                { server = options.serverOptions
                , net = options.netServerOptions
                }

  server.onStream $ \stream, headers => do
    let Right req = parseRequest stream headers
          | Left err => sendResponse (options.errorHandler err) stream
        initialRes = MkResponse OK [] () {h = StringHeaders}
        push = if stream.pushAllowed then pusher stream
                                     else const $ pure ()
        result = handler push $ MkContext req initialRes

    sendResponseFromPromise options.errorHandler result stream

  server.listen options.listenOptions
  pure server

export
listen' : HasIO io
   => HasIO pushIO
   => Error e
   => { auto http2 : HTTP2Module }
   -> (
        (Lazy PushContext -> pushIO ())
        -> Context Method SimpleURL Version StringHeaders Status StringHeaders (Publisher IO Error Buffer) ()
        -> Promise e IO $ Context Method SimpleURL Version StringHeaders Status StringHeaders b (Publisher IO Error Buffer)
      )
   -> io Http2Server
listen' {http2} handler = listen http2 defaultOptions handler

namespace Secure

  public export
  record Options where
    constructor MkOptions
    netServerOptions : Net.CreateServer.Options
    tlsServerOptions : TLS.CreateServer.Options
    tlsContextOptions : TLS.CreateSecureContext.Options
    serverOptions : HTTP2.CreateSecureServer.Options
    listenOptions : Listen.Options
    errorHandler : String -> RawHttpResponse

  export
  defaultOptions : HTTP2.Secure.Options
  defaultOptions = MkOptions
    { netServerOptions = Net.CreateServer.defaultOptions
    , tlsServerOptions = TLS.CreateServer.defaultOptions
    , tlsContextOptions = TLS.CreateSecureContext.defaultOptions
    , serverOptions = HTTP2.CreateSecureServer.defaultOptions
    , listenOptions =
      { port := Just 3443
      , host := Just "localhost"
      } Listen.defaultOptions
    , errorHandler = \e => MkResponse
      { status = INTERNAL_SERVER_ERROR
      , headers =
        [ ("Content-Type", "text/plain")
        , ("Content-Length", show $ length e)
        ]
      , body = singleton $ fromString e
      }
    }

  export
  listen : HasIO io
     => HasIO pushIO
     => Error e
     => HTTP2Module
     -> HTTP2.Secure.Options
     -> (
          (Lazy PushContext -> pushIO ())
          -> Context Method SimpleURL Version StringHeaders Status StringHeaders (Publisher IO Error Buffer) ()
          -> Promise e IO $ Context Method SimpleURL Version StringHeaders Status StringHeaders b (Publisher IO Error Buffer)
        )
     -> io Http2Server
  listen http2 options handler = do
    server <- http2.createSecureServer $ MkOptions
                  { server = options.serverOptions
                  , context = options.tlsContextOptions
                  , tls = options.tlsServerOptions
                  , net = options.netServerOptions
                  }

    server.onStream $ \stream, headers => do
      let Right req = parseRequest stream headers
            | Left err => sendResponse (options.errorHandler err) stream
          initialRes = MkResponse OK [] () {h = StringHeaders}
          push = if stream.pushAllowed then pusher stream
                                       else const $ pure ()
          result = handler push $ MkContext req initialRes

      sendResponseFromPromise options.errorHandler result stream

    server.listen options.listenOptions
    pure server
