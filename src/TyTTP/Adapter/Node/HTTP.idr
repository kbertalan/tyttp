module TyTTP.Adapter.Node.HTTP

import Data.Buffer
import Node
import Node.Error
import Node.HTTP.Server
import TyTTP
import TyTTP.HTTP as HTTP

public export
RawHttpRequest : Type
RawHttpRequest = HttpRequest { monad = IO, error = NodeError } String StringHeaders Buffer

public export
RawHttpResponse : Type
RawHttpResponse = Response Status StringHeaders $ Publisher IO NodeError Buffer

toNodeResponse : RawHttpResponse -> Node.HTTP.Server.ServerResponse -> IO ()
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
    mapHeaders h = do
      newHeaders <- empty
      foldlM (\hs, (k,v) => hs.setHeader k v) newHeaders h

fromPromiseToNodeResponse : Promise NodeError IO (Step Method String StringHeaders f Status StringHeaders b $ Publisher IO NodeError Buffer) -> ServerResponse -> IO ()
fromPromiseToNodeResponse (MkPromise cont) nodeRes =
  let callbacks = MkCallbacks
        { onSucceded = \a => toNodeResponse a.response nodeRes }
        { onFailed = \e => do
            h1 <- empty
            h2 <- h1.setHeader "Content-Type" "plain/text"
            headers <- h2.setHeader "Content-Length" $ show $ length e.message
            nodeRes.writeHead ((.code) INTERNAL_SERVER_ERROR) headers
            nodeRes.write e.message
            nodeRes.end
        }
  in
    cont callbacks

fromNodeRequest : Node.HTTP.Server.IncomingMessage -> RawHttpRequest
fromNodeRequest nodeReq =
  let method = parseMethod nodeReq.method
      path = nodeReq.url
      headers = nodeReq.headers.asList
  in HTTP.mkRequest method path headers $ HTTP.mkRequestBody method $ MkPublisher $ \s => do
        nodeReq.onData s.onNext
        nodeReq.onError s.onFailed
        nodeReq.onEnd s.onSucceded

export
listen : HasIO io
   => { auto http : HTTP }
   -> { default 3000 port : Int }
   -> ( 
    Step Method String StringHeaders (HTTP.bodyOf { monad = IO } {error = NodeError}) Status StringHeaders Buffer ()
     -> Promise NodeError IO $ Step Method String StringHeaders f Status StringHeaders b (Publisher IO NodeError Buffer)
  )
   -> io Server
listen {http} {port} handler = do
  server <- http.createServer

  server.onRequest $ \req => \res => do
    let handlerReq = fromNodeRequest req
        initialRes = MkResponse OK [] () {h = StringHeaders}
        result = handler $ MkStep handlerReq initialRes

    fromPromiseToNodeResponse result res

  server.listen port
  pure server

