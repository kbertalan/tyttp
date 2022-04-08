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

main : IO ()
main = do
  http <- HTTP2.require
  ignore $ HTTP2.listen' {e = String} $
      routes' (text "Resource could not be found" >=> status NOT_FOUND)
        [ get $ path "/query" $ \step =>
            text step.request.url.search step >>= status OK
        , get $ path "/parsed" $ Simple.search $ \step =>
            text (show step.request.url.search) step >>= status OK
        , get $ path "/request" :> \ctx => do
            putStrLn "Calling http"
            res <- MkPromise $ \cb =>
              ignore $ http.get "http://localhost:3000/parsed?q=from-request" cb.onSucceded

            if res.statusCode == 200
              then 
                pure $
                  { response.status := OK
                  , response.headers := [("Content-Type", "text/plain")]
                  , response.body := MkPublisher $ \s => do
                      onData res s.onNext
                      onEnd res s.onSucceded
                      onError res s.onFailed
                  } ctx
              else
                text "HTTP call failed with status code \{show res.statusCode}" ctx >>= status INTERNAL_SERVER_ERROR
        ]
