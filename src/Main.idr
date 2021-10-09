module Main

import Node
import Node.HTTP.Client
import Node.HTTP.Server
import TyTTP
import TyTTP.Adapter.Node
import TyTTP.Combinators
import Node.Error

main : IO ()
main = do
  http <- require
  server <- listenOnHttp http 3000 $ \i => hEcho i >>= hToPublisher

  defer $ do
    clientReq <- http.post "http://localhost:3000/" $ \res => do
      putStrLn "POST"
      putStrLn res.statusCode
      onData res putStrLn

    clientReq.write "Hello World!"
    clientReq.write "With more chunks"
    clientReq.end

  defer $ do 
    ignore $ http.get "http://localhost:3000/" $ \res => do
      putStrLn "GET"
      putStrLn res.statusCode
      onData res putStrLn
      server.close

