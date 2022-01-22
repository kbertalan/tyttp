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

sendError :
  Error e
  => HasIO io
  => Status
  -> String
  -> Step me u h1 s StringHeaders a b
  -> io $ Step me u h1 Status StringHeaders a (Publisher IO e Buffer)
sendError st str step = do
  text str step >>= status st

hQuery : Error e
  => Monad m
  => (src -> String)
  -> Step me (URL auth pth src) h1 s StringHeaders a ()
  -> m $ Step me (URL auth pth src) h1 Status StringHeaders a (Publisher IO e Buffer)
hQuery toString step = do
  let query = toString step.request.url.search
  text query step >>= status OK

hRouting : Error e
  => Step Method String StringHeaders Status StringHeaders (Publisher IO e Buffer) ()
  -> Promise e IO $ Step Method String StringHeaders Status StringHeaders (Publisher IO e Buffer) (Publisher IO e Buffer)
hRouting =
  let routingError = sendError NOT_FOUND "Resource could not be found"
      parseUrlError = \err => sendError BAD_REQUEST "URL has invalid format"
      decodeUriError = sendError BAD_REQUEST "URI decode has failed"
  in
    decodeUri' decodeUriError :> parseUrl' parseUrlError :> routes' routingError
        [ get $ path "/query" :>> hQuery id
        , get $ path "/parsed" :> Simple.search :> hQuery show
        ]

main : IO ()
main = eitherT putStrLn pure $ do
  http <- HTTP.require
  ignore $ HTTP.listen' $ hRouting

