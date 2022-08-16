module Echo

import Data.Buffer.Ext
import Node.HTTP
import Node.Timers
import TyTTP.Adapter.Node.HTTP as HTTP
import TyTTP.HTTP

hReflect : Context Method String Version StringHeaders Status StringHeaders (Publisher IO NodeError Buffer) ()
  -> IO $ Context Method String Version StringHeaders Status StringHeaders (Publisher IO NodeError Buffer) (Publisher IO NodeError Buffer)
hReflect ctx = do
  let m = ctx.request.method
      h = ctx.request.headers
      p : Publisher IO NodeError Buffer = MkPublisher $ \s => do
        s.onNext $ fromString "method -> \{show m}"
        s.onNext $ fromString "url -> \{ctx.request.url}"
        s.onNext $ fromString "version -> \{show ctx.request.version}"
        s.onNext "headers ->"
        for_ h $ \v => s.onNext $ fromString "\t\{fst v} : \{snd v}"
        s.onNext "body ->"
        ctx.request.body.subscribe s
  pure $ { response.body := p } ctx

main : IO ()
main = do
  http <- require
  server <- HTTP.listen' { e = NodeError } :> hReflect

  ignore $ setImmediate $ do
    ignore $ http.get "http://localhost:3000" defaultOptions $ \res => do
      putStrLn "GET"
      putStrLn $ show res.statusCode
      res.onData $ putStrLn . show

  ignore $ setImmediate $ do
    clientReq <- http.post "http://localhost:3000/the/resource" defaultOptions $ \res => do
      putStrLn "POST"
      putStrLn $ show res.statusCode
      res.onData $ putStrLn . show
      server.close

    clientReq.write "Hello World!" Nothing
    clientReq.write "With more chunks" Nothing
    clientReq.end Nothing { d = Buffer }

