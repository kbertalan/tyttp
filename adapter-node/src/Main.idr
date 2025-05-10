module Main

import Data.List

import TyTTP.Adapter.Node.HTTP
import TyTTP.HTTP
import TyTTP.URL

main : IO ()
main = do
  http <- Node.HTTP.require

  -- must set host to 0.0.0.0 if it is running in docker, otherwise
  -- let options = Node.HTTP.defaultOptions
  -- would be enough
  let options = { listenOptions :=
                    { port := Just 3000
                    , host := Just "0.0.0.0"
                    } Listen.defaultOptions
                } Node.HTTP.defaultOptions

  ignore $ listen http options { e = String }
    $ parseUrl' (const $ sendText "URL has invalid format" >=> status BAD_REQUEST)
    :> routes' (sendText "Resource could not be found" >=> status NOT_FOUND)
        [ get $ pattern "/query" $ \ctx => do
            putStrLn "serving query"
            sendText ctx.request.url.search ctx >>= status OK
        , get $ pattern "/parsed" $ Simple.search $ \ctx => do
            putStrLn "serving parsed"
            sendText (show ctx.request.url.search) ctx >>= status OK
        , get $ pattern "/" $ \ctx => do
            putStrLn "serving root"
            sendText "welcome adventurer" ctx >>= status OK
        , get $ pattern "/example/{id}" $ \ctx => do
            let maybeId = lookup "id" ctx.request.url.path.params
            putStrLn $ "parameters: \{show ctx.request.url.path.params}"
            sendText "id: \{show maybeId}" ctx >>= status OK
        , get $ pattern "/example/{id}/*" $ \ctx => do
            let maybeId = lookup "id" ctx.request.url.path.params
            putStrLn $ "parameters: \{show ctx.request.url.path.params} and rest: \{ctx.request.url.path.rest}"
            sendText "id: \{show maybeId} and rest: \{show ctx.request.url.path.rest}" ctx >>= status OK
        ]

  let Just port = options.listenOptions.port | Nothing => pure ()
  putStrLn $ "started server on port " <+> show port
