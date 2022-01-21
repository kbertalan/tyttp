module TyTTP.Adapter.Node.HTTP

import Data.Buffer
import Data.Buffer.Ext
import public Node.Error
import Node.HTTP.Server
import TyTTP
import public TyTTP.Adapter.Node.Error
import TyTTP.HTTP as HTTP

public export
RawHttpRequest : Type
RawHttpRequest = HttpRequest String StringHeaders $ Publisher IO NodeError Buffer

public export
RawHttpResponse : Type
RawHttpResponse = Response Status StringHeaders $ Publisher IO NodeError Buffer

toNodeResponse : RawHttpResponse -> Node.HTTP.Server.ServerResponse -> IO ()
toNodeResponse res nodeRes = do
  let status = res.status.code
  headers <- mapHeaders res.headers

  nodeRes.writeHead status headers
  res.body.subscribe $ MkSubscriber
        { onNext = \a => nodeRes.write a }
        { onFailed = \e => pure () }
        { onSucceded = \_ => nodeRes.end }
  where
    mapHeaders : StringHeaders -> IO Node.HTTP.Headers.Headers
    mapHeaders h = do
      newHeaders <- empty
      foldlM (\hs, (k,v) => hs.setHeader k v) newHeaders h

fromPromiseToNodeResponse : Error e
  => (e -> RawHttpResponse)
  -> Promise e IO (Step Method String StringHeaders Status StringHeaders b $ Publisher IO NodeError Buffer)
  -> ServerResponse
  -> IO ()
fromPromiseToNodeResponse errorHandler (MkPromise cont) nodeRes =
  let callbacks = MkCallbacks
        { onSucceded = \a => toNodeResponse a.response nodeRes }
        { onFailed = \e => toNodeResponse (errorHandler e) nodeRes }
  in
    cont callbacks

fromNodeRequest : Node.HTTP.Server.IncomingMessage -> RawHttpRequest
fromNodeRequest nodeReq =
  let method = parseMethod nodeReq.method
      path = nodeReq.url
      headers = nodeReq.headers.asList
  in HTTP.mkRequest method path headers $ MkPublisher $ \s => do
        nodeReq.onData s.onNext
        nodeReq.onError s.onFailed
        nodeReq.onEnd s.onSucceded

public export
record ListenOptions e where
  constructor MkListenOptions
  port : Int
  errorHandler : (e -> RawHttpResponse)

export
defaultListenOptions : Error e => ListenOptions e
defaultListenOptions = MkListenOptions
  { port = 3000
  , errorHandler = \e => MkResponse
    { status = INTERNAL_SERVER_ERROR
    , headers =
      [ ("Content-Type", "text/plain")
      , ("Content-Length", show $ length $ message e)
      ]
    , body = singleton $ fromString $ message e
    }
  }

export
listen : HasIO io
   => Error e
   => HTTP
   -> ListenOptions e
   -> ( 
    Step Method String StringHeaders Status StringHeaders (Publisher IO NodeError Buffer) ()
     -> Promise e IO $ Step Method String StringHeaders Status StringHeaders b (Publisher IO NodeError Buffer)
  )
   -> io Server
listen http options handler = do
  server <- http.createServer

  server.onRequest $ \req => \res => do
    let handlerReq = fromNodeRequest req
        initialRes = MkResponse OK [] () {h = StringHeaders}
        result = handler $ MkStep handlerReq initialRes

    fromPromiseToNodeResponse options.errorHandler result res

  server.listen options.port
  pure server

export
listen' : HasIO io
   => Error e
   => { auto http : HTTP }
   -> { default defaultListenOptions options : ListenOptions e }
   -> ( 
    Step Method String StringHeaders Status StringHeaders (Publisher IO NodeError Buffer) ()
     -> Promise e IO $ Step Method String StringHeaders Status StringHeaders b (Publisher IO NodeError Buffer)
  )
   -> io Server
listen' {http} {options} handler = listen http options handler
