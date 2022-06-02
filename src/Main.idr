module Main

import Data.Buffer
import Control.Monad.Trans
import Control.Monad.Maybe
import Node.HTTP2.Client
import Node.HTTP2.Server
import TyTTP.Adapter.Node.HTTP2
import TyTTP.HTTP
import TyTTP.HTTP.Producer
import TyTTP.HTTP.Routing
import TyTTP.URL
import TyTTP.URL.Path
import TyTTP.URL.Search

main : IO ()
main = do
  http2 <- HTTP2.require
  ignore $ HTTP2.listen' {e = String, pushIO = IO} $ \push =>
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
        ]

