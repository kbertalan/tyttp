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

hReflect : Step Method String StringHeaders Status StringHeaders (Publisher IO NodeError Buffer) ()
  -> IO $ Step Method String StringHeaders Status StringHeaders (Publisher IO NodeError Buffer) (Publisher IO NodeError Buffer)
hReflect step = do
  let m = step.request.method
      h = step.request.headers
      p : Publisher IO NodeError Buffer = MkPublisher $ \s => do
        s.onNext $ fromString "method -> \{show m}"
        s.onNext $ fromString "url -> \{step.request.url}"
        s.onNext "headers ->"
        for_ h $ \v => s.onNext $ fromString "\t\{fst v} : \{snd v}"
        s.onNext "body ->"
        step.request.body.subscribe s
  pure $ { response.body := p } step

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

