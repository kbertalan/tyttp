module Echo

import Data.Buffer.Ext
import Data.List
import Node.HTTP2
import Node.Timers
import TyTTP.Adapter.Node.HTTP2
import TyTTP
import TyTTP.HTTP.Protocol
import TyTTP.URL

hReflect : Context Method SimpleURL Version StringHeaders Status StringHeaders (Publisher IO Error Buffer) ()
  -> IO $ Context Method SimpleURL Version StringHeaders Status StringHeaders (Publisher IO Error Buffer) (Publisher IO Error Buffer)
hReflect ctx = do
  let m = ctx.request.method
      h = sort ctx.request.headers
      p : Publisher IO Error Buffer = MkPublisher $ \s => do
        s.onNext $ fromString "method -> \{show m}\n"
        s.onNext $ fromString "path -> \{ctx.request.url.path}\n"
        s.onNext $ fromString "headers ->\n"
        for_ h $ \v => s.onNext $ fromString "\t\{fst v} : \{snd v}\n"
        s.onNext $ fromString "body ->\n"
        ctx.request.body.subscribe s
  pure $ { response.body := p } ctx

main : IO ()
main = do
  http2 <- HTTP2.require
  server <- listen' { e = Error, pushIO = IO } $ \_, ctx => lift $ hReflect ctx

  ignore $ setImmediate $ do
    session <- http2.connect "http://localhost:3000" defaultOptions
    stream <- session.get "/" =<< Headers.empty
    stream.onResponse $ \headers => do
      putStrLn "GET"
      stream.onData $ putStr . show
      session.close

  ignore $ setImmediate $ do
    session <- http2.connect "http://localhost:3000" defaultOptions
    stream <- session.post "/the/resource" =<< Headers.empty
    stream.onResponse $ \headers => do
      putStrLn "POST"
      stream.onData $ putStr . show
      session.close
      server.close

    stream.write "Hello World!\n" Nothing
    stream.write "With more chunks\n" Nothing
    stream.end Nothing
