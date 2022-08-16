module Push

import Data.IORef
import Data.Buffer.Ext
import Node.HTTP2
import Node.Timers
import TyTTP.Adapter.Node.HTTP2
import TyTTP.HTTP
import TyTTP.URL

main : IO ()
main = do
  http2 <- HTTP2.require
  server <- listen' {e = String} $ \push =>
      routes' (sendText "Resource could not be found" >=> status NOT_FOUND)
        [ get $ pattern "/push" :> \step => do
            push $ MkContext
              { request = MkRequest
                { method = GET
                , url = MkURL (Just HTTP) (Just "localhost:3000") "/pushed.txt" ""
                , version = Version_2
                , headers = []
                , body = ()
                }
              , response = MkResponse
                { status = OK
                , headers = [ ("content-type", "text/plain") ]
                , body = singleton "this is pushed"
                }
              }
            sendText "this is the response" step >>= status OK
        ]

  ignore $ setImmediate $ do
    session <- http2.connect "http://localhost:3000" defaultOptions

    counter <- newIORef 2
    let closer = do
      modifyIORef counter (\x => x-1)
      count <- readIORef counter
      putStrLn "\nclose counter is at \{show count}"
      when (count <= 0) $ do
        putStrLn "closing session and server"
        session.close
        server.close

    session.onStream $ \stream, headers => do
      stream.onPush $ \headers => putStrLn $ show $ filter (\(a,b) => a /= "date") $ headers.asList
      putStrLn "PUSH"
      putStrLn $ show headers.asList
      onData stream putStr
      onEnd stream closer

    stream <- session.get "/push" =<< empty
    stream.onResponse $ \headers => do
      putStrLn "GET"
      onData stream putStr
      onEnd stream closer

    stream.end
