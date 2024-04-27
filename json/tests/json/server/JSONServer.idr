module JSONServer

import Data.Buffer.Ext
import JSON
import JSON.Derive
import TyTTP.Adapter.Node.HTTP
import TyTTP.Adapter.Node.URI
import TyTTP.HTTP
import TyTTP.HTTP.Consumer.JSON
import TyTTP.HTTP.Producer.JSON
import TyTTP.URL

%language ElabReflection

%hide JSON.Parser.JSON

record Example where
  constructor MkExample
  field : String
  opt : Maybe Int

%runElab derive "Example" [Show, Eq, ToJSON, FromJSON]

main : IO ()
main = do
  http <- HTTP.require
  server <- HTTP.listen'
    $ (\next, ctx => mapFailure message (next ctx))
    $ parseUrl' (const $ sendText "URL has invalid format" >=> status BAD_REQUEST)
    :> routes' (sendText "Resource could not be found" >=> status NOT_FOUND) { m = Promise Error IO }
        [ post
            $ pattern "/json"
            $ consumes' [JSON]
                { a = Example }
                (\ctx => sendText "Content cannot be parsed: \{ctx.request.body}" ctx >>= status BAD_REQUEST)
            $ \ctx => sendJSON ctx.request.body ctx >>= status OK
        ]

  req <- http.request "http://localhost:3000/json" ({ request.method := "POST", request.headers := Just (singleton "Content-Type" "application/json") } defaultOptions) $ \res => do
    res.onData $ putStrLn . show
    res.onEnd server.close

  req.write """
  {
    "field": "a field value",
    "opt": 1
  }
  """ Nothing
  req.end Nothing
