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

main : IO ()
main = do
  http <- HTTP.require
  ignore $ HTTP.listen' {e = String} $
      decodeUri' (text "URI decode has failed" >=> status BAD_REQUEST)
      :> parseUrl' (const $ text "URL has invalid format" >=> status BAD_REQUEST)
      :> routes' (text "Resource could not be found" >=> status NOT_FOUND)
          [ get $ path "/query" $ \step =>
              text step.request.url.search step >>= status OK
          , get $ path "/parsed" $ Simple.search $ \step =>
              text (show step.request.url.search) step >>= status OK
          ]
