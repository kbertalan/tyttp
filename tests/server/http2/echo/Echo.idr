module Echo

import Control.Monad.Trans
import Data.Buffer
import Data.Buffer.Ext
import Node
import Node.HTTP2.Client
import Node.HTTP2.Server
import TyTTP.Adapter.Node.HTTP2 as HTTP2
import TyTTP.Core.Error
import TyTTP.HTTP
import TyTTP.HTTP.Combinators
import TyTTP.URL

hReflect : Context Method SimpleURL Version StringHeaders Status StringHeaders (Publisher IO NodeError Buffer) ()
  -> IO $ Context Method SimpleURL Version StringHeaders Status StringHeaders (Publisher IO NodeError Buffer) (Publisher IO NodeError Buffer)
hReflect ctx = do
  let m = ctx.request.method
      h = ctx.request.headers
      p : Publisher IO NodeError Buffer = MkPublisher $ \s => do
        s.onNext $ fromString "method -> \{show m}\n"
        s.onNext $ fromString "path -> \{ctx.request.url.path}\n"
        s.onNext $ fromString "headers ->\n"
        for_ h $ \v => s.onNext $ fromString "\t\{fst v} : \{snd v}\n"
        s.onNext $ fromString "body ->\n"
        ctx.request.body.subscribe s
  pure $ { response.body := p } ctx

main : IO ()
main = do
  http2 <- require
  server <- HTTP2.listen' { e = NodeError, pushIO = IO } $ \_, ctx => lift $ hReflect ctx

  defer $ do
    session <- http2.connect "http://localhost:3000" defaultOptions
    stream <- session.get "/" =<< Headers.empty
    stream.onResponse $ \headers => do
      putStrLn "GET"
      onData stream putStr
      session.close

  defer $ do
    session <- http2.connect "http://localhost:3000" defaultOptions
    stream <- session.post "/the/resource" =<< Headers.empty 
    stream.onResponse $ \headers => do
      putStrLn "POST"
      onData stream $ putStr
      session.close
      server.close

    stream.write "Hello World!\n"
    stream.write "With more chunks\n"
    stream.end

