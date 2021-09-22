module TyTTP.Adapter.Node

import Node.HTTP.Server
import TyTTP

public export
StringHeaders : Type
StringHeaders = List (String, String)

toNodeResponse : Response StringHeaders (Publisher IO error String) -> Node.HTTP.Server.ServerResponse -> IO ()
toNodeResponse res nodeRes = do
  let status = mapStatus res.status
  headers <- mapHeaders res.headers

  nodeRes.writeHead status headers
  res.body.subscribe $ MkSubscriber
        { onNext = \a => nodeRes.write a }
        { onFailed = \e => pure () }
        { onSucceded = \_ => nodeRes.end }
  where
    mapStatus : TyTTP.Response.Status -> Int
    mapStatus s = case s of
      OK => 200
      Moved => 302
      BadRequest => 404
      InternalError => 500

    mapHeaders : StringHeaders -> IO Node.HTTP.Headers.Headers
    mapHeaders h = foldlM (\hs, (k,v) => hs.setHeader k v) empty h

fromNodeRequest : Node.HTTP.Server.IncomingMessage -> Request StringHeaders (Publisher IO error a)
fromNodeRequest nodeReq =
  MkRequest [] $ MkPublisher $ \s => do
      nodeReq.onData s.onNext
      nodeReq.onError s.onFailed
      nodeReq.onEnd s.onSucceded

export
listenOnHttp : HTTP -> Int -> Handler IO StringHeaders StringHeaders (Publisher IO () String) () StringHeaders StringHeaders a (Publisher IO () String) -> IO Server
listenOnHttp http port handler = do
  server <- http.createServer

  server.onRequest $ \req => \res => do
    let handlerReq = fromNodeRequest {error = ()} req
        initialRes = MkResponse OK [] () {h = StringHeaders}

    result <- handler $ MkStep handlerReq initialRes

    toNodeResponse result.response res

  server.listen port
  pure server

