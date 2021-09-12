module Main

import Stream
import Node.HTTP

main : IO ()
main = do
  http <- require
  server <- http.createServer

  server.onRequest $ \req => \res => do
    let headers = singleton "Content-Type" "text/plain"
    res.writeHead 200 headers

    res.write "Hello World!"
 
    res.end
    server.close

--  server.listen 3000

  putStrLn "OK"


