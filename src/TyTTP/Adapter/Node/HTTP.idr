module TyTTP.Adapter.Node.HTTP

import public Data.Buffer
import Data.Buffer.Ext
import public Node.Error
import public Node.HTTP
import public Node.Net.Server.Listen
import TyTTP
import public TyTTP.Adapter.Node.Error
import TyTTP.HTTP

public export
RawHttpRequest : Type
RawHttpRequest = HttpRequest String StringHeaders $ Publisher IO Error Buffer

public export
RawHttpResponse : Type
RawHttpResponse = Response Status StringHeaders $ Publisher IO Error Buffer

toNodeResponse : RawHttpResponse -> ServerResponse -> IO ()
toNodeResponse res nodeRes = do
  let status = res.status.code
  headers <- mapHeaders res.headers

  nodeRes.writeHead status headers
  res.body.subscribe $ MkSubscriber
        { onNext = \a => nodeRes.write a Nothing }
        { onFailed = \e => pure () }
        { onSucceded = \_ => nodeRes.end Nothing { d = Buffer } }
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

public export
record Options e where
  constructor MkOptions
  listenOptions : Listen.Options
  serverOptions : HTTP.CreateServer.Options
  errorHandler : (e -> RawHttpResponse)

export
defaultOptions : Error e => Adapter.Node.HTTP.Options e
defaultOptions = MkOptions
  { listenOptions =
    { port := Just 3000
    , host := Just "localhost"
    } Listen.defaultOptions
  , serverOptions = HTTP.CreateServer.defaultOptions
  , errorHandler = \e => MkResponse
    { status = INTERNAL_SERVER_ERROR
    , headers =
      [ ("Content-Type", "text/plain")
      , ("Content-Length", show $ length $ TyTTP.Core.Error.message e)
      ]
    , body = singleton $ fromString $ TyTTP.Core.Error.message e
    }
  }

export
listen : HasIO io
   => Error e
   => HTTP
   -> Adapter.Node.HTTP.Options e
   -> ( 
    Context Method String Version StringHeaders Status StringHeaders (Publisher IO Error Buffer) ()
     -> Promise e IO $ Context Method String Version StringHeaders Status StringHeaders b (Publisher IO Error Buffer)
  )
   -> io Server
listen http options handler = do
  server <- http.createServer options.serverOptions

  server.onRequest $ \req => \res => do
    let handlerReq = fromNodeRequest req
        initialRes = MkResponse OK [] () {h = StringHeaders}
        result = handler $ MkContext handlerReq initialRes

    fromPromiseToNodeResponse options.errorHandler result res

  server.listen options.listenOptions
  pure server

export
listen' : HasIO io
   => Error e
   => { auto http : HTTP }
   -> ( 
    Context Method String Version StringHeaders Status StringHeaders (Publisher IO Error Buffer) ()
     -> Promise e IO $ Context Method String Version StringHeaders Status StringHeaders b (Publisher IO Error Buffer)
  )
   -> io Server
listen' {http} handler = listen http defaultOptions handler
