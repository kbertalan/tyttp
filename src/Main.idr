module Main

import Control.Monad.Either
import Control.Monad.Maybe
import Control.Monad.Trans
import Data.Buffer
import Node.HTTPS.Server
import System
import System.File.ReadWrite
import TyTTP.Adapter.Node.HTTPS
import TyTTP.HTTP
import TyTTP.HTTP.Producer
import TyTTP.HTTP.Routing
import TyTTP.URL
import TyTTP.URL.Path
import TyTTP.URL.Search

%hide TyTTP.HTTP.Routing.ContentType.text
%hide TyTTP.URL.URL.path

main : IO ()
main = do
  Just keyFile <- getEnv "KEY_FILE"
    | Nothing => putStrLn "Environment variable \"KEY_FILE\" is not set"
  Right key <- readFile keyFile
    | Left e => putStrLn "Could not read file \{keyFile}, reason: \{show e}"

  Just certFile <- getEnv "CERT_FILE"
    | Nothing => putStrLn "environment variable \"CERT_FILE\" is not set"
  Right cert <- readFile certFile
    | Left e => putStrLn "Could not read file \{certFile}, reason: \{show e}"

  let options = { tlsContextOptions.cert := [cert]
                , tlsContextOptions.key := [key]
                } HTTPS.defaultOptions

  https <- HTTPS.require
  ignore $ listen https options { e = String }
    $ parseUrl' (const $ text "URL has invalid format" >=> status BAD_REQUEST)
    :> routes' (text "Resource could not be found" >=> status NOT_FOUND)
        [ get $ path "/query" $ \ctx =>
            text ctx.request.url.search ctx >>= status OK
        , get $ path "/parsed" $ Simple.search $ \ctx =>
            text (show ctx.request.url.search) ctx >>= status OK
        ]

