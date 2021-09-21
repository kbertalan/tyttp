module Main

import Data.Buffer
import Data.IORef
import Handler
import Handler.Combinators
import Node
import Node.HTTP
import Node.HTTP.Get
import Stream

StringHeaders : Type
StringHeaders = List (String, String)

toNodeResponse : Handler.Response.Response StringHeaders (Publisher IO error String) -> Node.HTTP.Server.ServerResponse -> IO ()
toNodeResponse res nodeRes = do
  let status = mapStatus res.status
  headers <- mapHeaders res.headers

  nodeRes.writeHead status headers
  res.body.subscribe $ MkSubscriber
        { onNext = \a => nodeRes.write a }
        { onFailed = \e => pure () }
        { onSucceded = \_ => nodeRes.end }
  where
    mapStatus : Handler.Response.Status -> Int
    mapStatus s = case s of
      OK => 200
      Moved => 302
      BadRequest => 404
      InternalError => 500

    mapHeaders : StringHeaders -> IO Node.HTTP.Headers.Headers
    mapHeaders h = foldlM (\hs, (k,v) => hs.setHeader k v) empty h

fromNodeRequest : Node.HTTP.Server.IncomingMessage -> Handler.Request.Request StringHeaders (Publisher IO error a)
fromNodeRequest nodeReq =
  MkRequest [] $ MkPublisher $ \s => do
      nodeReq.onData s.onNext
      nodeReq.onError s.onFailed
      nodeReq.onEnd s.onSucceded

main : IO ()
main = do
  http <- require
  server <- http.createServer

  let handler = hEcho

  server.onRequest $ \req => \res => do
    let handlerReq = fromNodeRequest {error = ()} req
        initialRes = MkResponse OK ([] { a = (String, String)}) ()

    result <- handler $ MkStep handlerReq initialRes

    toNodeResponse result.response res
 
    server.close

  server.listen 3000

  clientReq <- http.post "http://localhost:3000/" $ \res => putStrLn $ toJsonString res.headers
  clientReq.write "Hello World!"
  clientReq.end

  putStrLn "OK"


