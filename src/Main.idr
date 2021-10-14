module Main

import Node
import Node.HTTP.Client
import Node.HTTP.Server
import TyTTP
import TyTTP.Adapter.Node.HTTP as HTTP
import TyTTP.Combinators
import TyTTP.Combinators.HTTP
import TyTTP.HTTP
import Node.Error

hReflect : Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) StringHeaders String ()
  -> IO $ Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) StringHeaders String (Publisher IO NodeError String)
hReflect step = do
  let m = step.request.method
      h = step.request.headers
      p = MkPublisher { m = IO } { e = NodeError } $ \s => do
        s.onNext "method -> \{show m}"
        s.onNext "path -> \{step.request.path}"
        s.onNext "headers ->"
        for_ h $ \v => s.onNext "\t\{fst v} : \{snd v}"
        s.onNext "body ->"
        selectBodyByMethod m (s.onNext "empty") $
          (believe_me step.request.body).subscribe s
  hConstResponse p step
  

main : IO ()
main = do
  http <- require
  server <- HTTP.listen hReflect

  defer $ do
    clientReq <- http.post "http://localhost:3000/the/resource" $ \res => do
      putStrLn "POST"
      putStrLn res.statusCode
      onData res putStrLn
      server.close

    clientReq.write "Hello World!"
    clientReq.write "With more chunks"
    clientReq.end

