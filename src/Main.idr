module Main

import System
import System.File.ReadWrite
import TyTTP.Adapter.Node.HTTPS
import TyTTP.HTTP
import TyTTP.URL

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
    $ parseUrl' (const $ sendText "URL has invalid format" >=> status BAD_REQUEST)
    :> routes' (sendText "Resource could not be found" >=> status NOT_FOUND)
        [ get $ pattern "/query" $ \ctx =>
            sendText ctx.request.url.search ctx >>= status OK
        , get $ pattern "/parsed" $ Simple.search $ \ctx =>
            sendText (show ctx.request.url.search) ctx >>= status OK
        ]

