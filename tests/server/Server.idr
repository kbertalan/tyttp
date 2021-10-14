module Server

import Node
import Node.HTTP.Client
import Node.HTTP.Server
import TyTTP
import TyTTP.Adapter.Node.HTTP as HTTP
import TyTTP.Combinators
import TyTTP.Combinators.HTTP
import TyTTP.HTTP
import Node.Error

main : IO ()
main = do
  http <- require
  server <- HTTP.listen $ \i => hEcho i >>= hToPublisher

  defer $ do
    clientReq <- http.post "http://localhost:3000/" $ \res => do
      putStrLn "POST"
      putStrLn res.statusCode
      onData res putStrLn
      server.close

    clientReq.write "Hello World!"
    clientReq.write "With more chunks"
    clientReq.end
