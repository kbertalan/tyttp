module Main

import Handler
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


main : IO ()
main = do
  http <- require
  server <- http.createServer

  server.onRequest $ \req => \res => do
    let handlerRes = MkResponse OK [( "Content-Type", "text/plain" )] $ MkPublisher {e = ()} $ \s => s.onNext "Hello World!" >>= s.onSucceded

    toNodeResponse handlerRes res
 
    server.close

  server.listen 3000

  ignore $ http.get "http://localhost:3000/" $ \res => putStrLn $ toJsonString res.headers

  putStrLn "OK"


