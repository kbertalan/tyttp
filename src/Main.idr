module Main

import Data.Buffer
import Control.Monad.Trans
import Control.Monad.Either
import Control.Monad.Maybe
import Node.HTTP2.Client
import Node.HTTP2.Server
import TyTTP.Adapter.Node.HTTP2
import TyTTP.Adapter.Node.URI
import TyTTP.HTTP
import TyTTP.HTTP.Consumer
import TyTTP.HTTP.Producer
import TyTTP.HTTP.Routing
import TyTTP.URL
import TyTTP.URL.Path
import TyTTP.URL.Search


import Node.HTTP2.Client
import Node.Headers
import Node
import Data.IORef
import Data.Buffer.Ext

main : IO ()
main = do
  http2 <- HTTP2.require
  server <- HTTP2.listen' {e = String} $ \push =>
      routes' (text "Resource could not be found" >=> status NOT_FOUND)
        [ get $ path "/query" $ \step =>
            text step.request.url.search step >>= status OK
        , get $ path "/parsed" $ Simple.search $ \step =>
            text (show step.request.url.search) step >>= status OK
        , get $ path "/request" :> \ctx => do
            putStrLn "Calling http"
            (headers, stream) <- MkPromise $ \cb => do
              session <- http2.connect "http://localhost:3000"
              stream <- session.get "/parsed?q=from-request" =<< empty
              stream.onResponse $ \headers => cb.onSucceded (headers, stream)
              onError stream $ \error => cb.onFailed error

            pure $
              { response.status := OK
              , response.headers := [("Content-Type", "text/plain")]
              , response.body := MkPublisher $ \s => do
                  onData stream s.onNext
                  onEnd stream $ s.onSucceded ()
                  onError stream s.onFailed
              } ctx
        , get $ path "/push" :> \step => do
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
            text "this is the response" step >>= status OK
        ]

  defer $ do
    session <- http2.connect "http://localhost:3000"

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
      stream.onPush $ \headers => putStrLn $ show headers.asList
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

    -- session.close
    -- server.close
