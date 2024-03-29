module TyTTP.Adapter.Node.HTTPS

import public Data.Buffer
import Data.Buffer.Ext
import Data.String
import Data.Maybe
import public Node.Error
import Node.HTTP
import public Node.HTTPS
import TyTTP
import public TyTTP.Adapter.Node.Error
import TyTTP.HTTP
import TyTTP.URL

%hide Node.HTTP.Server.Server
%hide Node.Net.Server.Server

public export
RawHttpRequest : Type
RawHttpRequest = HttpRequest String StringHeaders $ Publisher IO Error Buffer

public export
RawHttpResponse : Type
RawHttpResponse = Response Status StringHeaders $ Publisher IO Error Buffer

public export
record Options e where
  constructor MkOptions
  netServerOptions : Net.CreateServer.Options
  tlsServerOptions : TLS.CreateServer.Options
  tlsContextOptions : TLS.CreateSecureContext.Options
  serverOptions : HTTPS.CreateServer.Options
  listenOptions : Listen.Options
  errorHandler : e -> RawHttpResponse

export
defaultOptions : Error e => HTTPS.Options e
defaultOptions = MkOptions
  { netServerOptions = Net.CreateServer.defaultOptions
  , tlsServerOptions = TLS.CreateServer.defaultOptions
  , tlsContextOptions = TLS.CreateSecureContext.defaultOptions
  , serverOptions = HTTPS.CreateServer.defaultOptions
  , listenOptions =
    { port := Just 3443
    , host := Just "localhost"
    } Listen.defaultOptions
  , errorHandler = \e => MkResponse
    { status = INTERNAL_SERVER_ERROR
    , headers =
      [ ("Content-Type", "text/plain")
      , ("Content-Length", show $ length $ TyTTP.Core.Error.message e)
      ]
    , body = singleton $ fromString $ TyTTP.Core.Error.message e
    }
  }

toNodeResponse : RawHttpResponse -> ServerResponse -> IO ()
toNodeResponse res nodeRes = do
  let status = res.status.code
  headers <- mapHeaders res.headers

  nodeRes.writeHead status headers
  res.body.subscribe $ MkSubscriber
        { onNext = \a => nodeRes.write a Nothing }
        { onFailed = \e => pure () }
        { onSucceded = \_ => nodeRes.end Nothing {d = Buffer} }
  where
    mapHeaders : StringHeaders -> IO Headers
    mapHeaders h = do
      newHeaders <- empty
      foldlM (\hs, (k,v) => hs.setHeader k v) newHeaders h

fromPromiseToNodeResponse : Error e
  => (e -> RawHttpResponse)
  -> Promise e IO (Context Method String Version StringHeaders Status StringHeaders b $ Publisher IO Error Buffer)
  -> ServerResponse
  -> IO ()
fromPromiseToNodeResponse errorHandler (MkPromise cont) nodeRes =
  let callbacks = MkCallbacks
        { onSucceded = \a => toNodeResponse a.response nodeRes }
        { onFailed = \e => toNodeResponse (errorHandler e) nodeRes }
  in
    cont callbacks

fromNodeRequest : IncomingMessage -> RawHttpRequest
fromNodeRequest nodeReq =
  let method = parseMethod nodeReq.method
      path = nodeReq.url
      headers = nodeReq.headers.asList
      version = parseVersion nodeReq.httpVersion
  in mkRequest method path version headers $ MkPublisher $ \s => do
        nodeReq.onData s.onNext
        nodeReq.onError s.onFailed
        nodeReq.onEnd $ s.onSucceded ()

export
listen : HasIO io
   => Error e
   => HTTPSModule
   -> HTTPS.Options e
   -> (
        Context Method String Version StringHeaders Status StringHeaders (Publisher IO Error Buffer) ()
        -> Promise e IO $ Context Method String Version StringHeaders Status StringHeaders b (Publisher IO Error Buffer)
      )
   -> io Server
listen https options handler = do
  server <- https.createServer $ MkOptions
              { server = options.serverOptions
              , context = options.tlsContextOptions
              , tls = options.tlsServerOptions
              , net = options.netServerOptions
              }

  server.onRequest $ \req => \res => do
    let handlerReq = fromNodeRequest req
        initialRes = MkResponse OK [] () {h = StringHeaders}
        result = handler $ MkContext handlerReq initialRes

    fromPromiseToNodeResponse options.errorHandler result res

  server.listen options.listenOptions
  pure server
