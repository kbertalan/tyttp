module Main

import Stream
import Node.HTTP

main : IO ()
main = do
  http <- require
  server <- http.createServer

  server.onRequest $ \req => \res => do
    putStrLn "Request received"
    res.end
    server.close

  server.listen 3000

  putStrLn "OK"


