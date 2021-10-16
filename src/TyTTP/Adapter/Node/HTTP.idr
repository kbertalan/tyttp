module TyTTP.Adapter.Node.HTTP

import Data.Buffer
import Node
import Node.Error
import Node.HTTP.Server
import TyTTP
import TyTTP.HTTP as HTTP

public export
StringHeaders : Type
StringHeaders = List (String, String)

public export
RawHttpRequest : { auto monad : Type -> Type } -> { auto error : Type } -> Type
RawHttpRequest = HttpRequest { monad } { error } String StringHeaders Buffer

public export
RawHttpResponse : { auto monad : Type -> Type } -> { auto error : Type } -> Type
RawHttpResponse = Response Status StringHeaders $ Publisher monad error Buffer

toNodeResponse : Error e => RawHttpResponse { monad = IO } { error = e } -> Node.HTTP.Server.ServerResponse -> IO ()
toNodeResponse res nodeRes = do
  let status = res.status.code
  headers <- mapHeaders res.headers

  nodeRes.writeHead status headers
  res.body.subscribe $ MkSubscriber
        { onNext = \a => nodeRes.write a }
        { onFailed = \e => pure () }
        { onSucceded = \_ => nodeRes.end }
  where
    mapHeaders : StringHeaders -> IO Node.HTTP.Headers.Headers
    mapHeaders h = foldlM (\hs, (k,v) => hs.setHeader k v) empty h

fromNodeRequest : Node.HTTP.Server.IncomingMessage -> RawHttpRequest { monad = IO } { error = NodeError }
fromNodeRequest nodeReq =
  let method = parseMethod nodeReq.method
      path = nodeReq.url
      headers = nodeReq.headers.asList
  in HTTP.mkRequest method path headers $ HTTP.mkRequestBody method $ MkPublisher $ \s => do
        nodeReq.onData s.onNext
        nodeReq.onError s.onFailed
        nodeReq.onEnd s.onSucceded

export
listen : 
   {auto http : HTTP}
   -> { default 3000 port : Int }
   -> ( Step Method String StringHeaders (HTTP.bodyOf { monad = IO } {error = NodeError}) Status StringHeaders Buffer ()
     -> IO $ Step Method String StringHeaders f Status StringHeaders b (Publisher IO NodeError Buffer))
   -> IO Server
listen {http} {port} handler = do
  server <- http.createServer

  server.onRequest $ \req => \res => do
    let handlerReq = fromNodeRequest req
        initialRes = MkResponse OK [] () {h = StringHeaders}

    result <- handler $ MkStep handlerReq initialRes

    toNodeResponse result.response res

  server.listen port
  pure server

