module Server

import Data.Buffer
import Node
import Node.Buffer
import Node.Error
import Node.HTTP.Client
import Node.HTTP.Server
import TyTTP.Adapter.Node.HTTP as HTTP
import TyTTP.HTTP
import TyTTP.HTTP.Combinators
import TyTTP.Support.Combinators

hReflect : Step Method String StringHeaders (HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer ()
  -> IO $ Step Method String StringHeaders (HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer (Publisher IO NodeError Buffer)
hReflect step = do
  let m = step.request.method
      h = step.request.headers
      p = MkPublisher { m = IO } { e = NodeError } { a = Buffer } $ \s => do
        s.onNext $ fromString "method -> \{show m}"
        s.onNext $ fromString "url -> \{step.request.url}"
        s.onNext "headers ->"
        for_ h $ \v => s.onNext $ fromString "\t\{fst v} : \{snd v}"
        s.onNext "body ->"
        selectBodyByMethod m (s.onNext "empty" >>= s.onSucceded) $
          (believe_me step.request.body).subscribe s
  hConstResponse { m = IO } p step

main : IO ()
main = do
  http <- require
  server <- HTTP.listen hReflect

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

