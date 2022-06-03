module Main

import Data.Buffer
import Control.Monad.Trans
import Control.Monad.Maybe
import Node.HTTP2.Client
import Node.HTTP2.Server
import System
import System.File.ReadWrite
import TyTTP.Adapter.Node.HTTP2
import TyTTP.HTTP
import TyTTP.HTTP.Producer
import TyTTP.HTTP.Routing
import TyTTP.URL
import TyTTP.URL.Path
import TyTTP.URL.Search

main : IO ()
main = do
  Just keyFile <- getEnv "KEY_FILE"
    | Nothing => putStrLn "Environment variable \"KEY_FILE\" is not set"
  Just certFile <- getEnv "CERT_FILE"
    | Nothing => putStrLn "environment variable \"CERT_FILE\" is not set"
  Right key <- readFile keyFile
    | Left e => putStrLn "Could not read file \{keyFile}, reason: \{show e}"
  Right cert <- readFile certFile
    | Left e => putStrLn "Could not read file \{certFile}, reason: \{show e}"
  let secureOptions = MkSecureOptions key cert

  http2 <- HTTP2.require
  ignore $ HTTP2.Secure.secureListen' secureOptions {e = String, pushIO = IO} $ \push =>
      routes' (text "Resource could not be found" >=> status NOT_FOUND)
        [ get $ path "/query" $ \ctx =>
            text ctx.request.url.search ctx >>= status OK
        , get $ path "/parsed" $ Simple.search $ \ctx =>
            text (show ctx.request.url.search) ctx >>= status OK
        , get $ path "/request" :> \ctx => do
            putStrLn "Calling https"
            (headers, stream) <- MkPromise $ \cb => do
              session <- http2.connect "https://localhost:3000" $ {
                  rejectUnauthorized := True
                } Connect.defaultOptions
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

