module Echo

import Control.Monad.Trans
import Data.Buffer
import Data.Buffer.Ext
import Node
import Node.HTTP.Client
import Node.HTTP.Server
import TyTTP.Adapter.Node.HTTP as HTTP
import TyTTP.Core.Error
import TyTTP.HTTP
import TyTTP.HTTP.Combinators

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

  defer $ do
    ignore $ http.get "http://localhost:3000" $ \res => do
      putStrLn "GET"
      putStrLn res.statusCode
      onData res putStrLn

  defer $ do
    clientReq <- http.post "http://localhost:3000/the/resource" $ \res => do
      putStrLn "POST"
      putStrLn res.statusCode
      onData res putStrLn
      server.close

    clientReq.write "Hello World!"
    clientReq.write "With more chunks"
    clientReq.end

