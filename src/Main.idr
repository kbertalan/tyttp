module Main

import Data.Buffer
import Control.Monad.Trans
import Control.Monad.Either
import Control.Monad.Maybe
import Node.HTTP.Server
import TyTTP.Adapter.Node.HTTP
import TyTTP.Adapter.Node.URI
import TyTTP.HTTP
import TyTTP.HTTP.Consumer
import TyTTP.HTTP.Producer
import TyTTP.HTTP.Routing
import TyTTP.URL
import TyTTP.URL.Path
import TyTTP.URL.Search

import Node.HTTP.Client
import Node

main : IO ()
main = do
  http <- HTTP.require
  server <- HTTP.listen' {e = String} $
      decodeUri' (text "URI decode has failed" >=> status BAD_REQUEST)
      :> parseUrl' (const $ text "URL has invalid format" >=> status BAD_REQUEST)
      :> routes' (text "Resource could not be found" >=> status NOT_FOUND)
          [ get $ path "/query" $ \ctx =>
              text ctx.request.url.search ctx >>= status OK
          , get $ path "/parsed" $ Simple.search $ \ctx =>
              text (show ctx.request.url.search) ctx >>= status OK
          , get $ path "/request" $ \ctx =>
              pure $
                { response.status := OK
                , response.body := MkPublisher $ \s => do
                    putStrLn "Calling http"
                    ignore $ http.get "http://localhost:3000/parsed?q=from-request" $ \res => do
                      putStrLn "Got response"
                      onData res s.onNext
                      onEnd res s.onSucceded
                } ctx
          ]

  putStrLn "Calling from main thread"
  ignore $ http.get "http://localhost:3000/request" $ \res => do
    putStrLn "response"
    onData res putStrLn
    server.close
