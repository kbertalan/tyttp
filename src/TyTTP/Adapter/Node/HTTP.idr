module TyTTP.Adapter.Node.HTTP

import Node.Error
import Node.HTTP.Server
import TyTTP
import TyTTP.HTTP as HTTP

public export
StringHeaders : Type
StringHeaders = List (String, String)

toNodeResponse : Error e => Response StringHeaders (Publisher IO e String) -> Node.HTTP.Server.ServerResponse -> IO ()
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
    mapHeaders h = foldlM (\hs, (k,v) => hs.setHeader k v) empty h

fromNodeRequest : Node.HTTP.Server.IncomingMessage -> Request Method StringHeaders (HTTP.bodyOf { monad = IO } { error = NodeError }) String
fromNodeRequest nodeReq =
  let method = parseMethod nodeReq.method
  in HTTP.mkRequest method [] $ HTTP.mkRequestBody { b = String } method $ MkPublisher $ \s => do
        nodeReq.onData s.onNext
        nodeReq.onError s.onFailed
        nodeReq.onEnd s.onSucceded

export
listen : 
   {auto http : HTTP}
   -> { default 3000 port : Int }
   -> Handler IO 
     Method StringHeaders (HTTP.bodyOf { monad = IO } {error = NodeError}) StringHeaders String ()
     Method StringHeaders f StringHeaders b (Publisher IO NodeError String)
   -> IO Server
listen {http} {port} handler = do
  server <- http.createServer

  server.onRequest $ \req => \res => do
    let handlerReq = fromNodeRequest req
        initialRes = MkResponse OK [] () {h = StringHeaders}

    result <- handler $ MkStep handlerReq initialRes

    toNodeResponse result.response res

  server.listen port
  pure server

