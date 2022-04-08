module TyTTP.Adapter.Node.HTTP2

import Data.Buffer
import Data.Buffer.Ext
import Data.String
import public Node.Error
import Node.HTTP2.Server
import TyTTP
import TyTTP.URL
import public TyTTP.Adapter.Node.Error
import TyTTP.HTTP as HTTP

namespace Fields

  public export
  data RequestPseudoHeaderField
    = Method
    | Scheme
    | Authority
    | Path

  public export
  Show RequestPseudoHeaderField where
    show f = case f of
      Method => ":method"
      Scheme => ":scheme"
      Authority => ":authority"
      Path => ":path"

  public export
  data ResponsePseudoHeaderField
    = Status

  public export
  Show ResponsePseudoHeaderField where
    show Status = ":status"

public export
RawHttpRequest : Type
RawHttpRequest = HttpRequest SimpleURL StringHeaders $ Publisher IO NodeError Buffer

public export
RawHttpResponse : Type
RawHttpResponse = Response Status StringHeaders $ Publisher IO NodeError Buffer

sendResponse : RawHttpResponse -> ServerHttp2Stream -> IO ()
sendResponse res stream = do
  let status = res.status.code
  headers <- mapHeaders res.headers status

  stream.respond headers
  res.body.subscribe $ MkSubscriber
        { onNext = \a => stream.write a }
        { onFailed = \e => pure () }
        { onSucceded = \_ => stream.end }
  where
    mapHeaders : StringHeaders -> Int -> IO Headers
    mapHeaders h s = do
      let newHeaders = singleton (show Fields.Status) (show s)
      foldlM (\hs, (k,v) => hs.setHeader k v) newHeaders h

sendResponseFromPromise : Error e
  => (String -> RawHttpResponse)
  -> Promise e IO (Context Method SimpleURL StringHeaders Status StringHeaders b $ Publisher IO NodeError Buffer)
  -> ServerHttp2Stream
  -> IO ()
sendResponseFromPromise errorHandler (MkPromise cont) stream =
  let callbacks = MkCallbacks
        { onSucceded = \a => sendResponse a.response stream }
        { onFailed = \e => sendResponse (errorHandler $ message e) stream }
  in
    cont callbacks

parseRequest : Node.HTTP2.Server.ServerHttp2Stream -> Headers -> Either String RawHttpRequest
parseRequest stream headers =
  let Just method = parseMethod <$> headers.getHeader (show Fields.Method)
        | Nothing => Left "Method header is missing from request"
      scheme = parseScheme <$> headers.getHeader (show Fields.Scheme)
      authority = headers.getHeader (show Fields.Authority)
      Just pathAndSearch = headers.getHeader (show Fields.Path)
        | Nothing => Left "Path header is missing from request"
      (path, search) = String.break (=='?') pathAndSearch
      url = MkURL scheme authority path search
  in Right $ HTTP.mkRequest method url headers.asList $ MkPublisher $ \s => do
        stream.onData s.onNext
        stream.onError s.onFailed
        stream.onEnd s.onSucceded

public export
record ListenOptions where
  constructor MkListenOptions
  port : Int
  errorHandler : (String -> RawHttpResponse)

export
defaultListenOptions : ListenOptions
defaultListenOptions = MkListenOptions
  { port = 3000
  , errorHandler = \e => MkResponse
    { status = INTERNAL_SERVER_ERROR
    , headers =
      [ ("Content-Type", "text/plain")
      , ("Content-Length", show $ length e)
      ]
    , body = singleton $ fromString e
    }
  }

export
listen : HasIO io
   => Error e
   => HTTP2
   -> ListenOptions
   -> ( 
    Context Method SimpleURL StringHeaders Status StringHeaders (Publisher IO NodeError Buffer) ()
     -> Promise e IO $ Context Method SimpleURL StringHeaders Status StringHeaders b (Publisher IO NodeError Buffer)
  )
   -> io Http2Server
listen http options handler = do
  server <- http.createServer

  server.onStream $ \stream => \headers => do
    let Right req = parseRequest stream headers
          | Left err => sendResponse (options.errorHandler err) stream
        initialRes = MkResponse OK [] () {h = StringHeaders}
        result = handler $ MkContext req initialRes

    sendResponseFromPromise options.errorHandler result stream

  server.listen options.port
  pure server

export
listen' : HasIO io
   => Error e
   => { auto http : HTTP2 }
   -> { default defaultListenOptions options : ListenOptions }
   -> ( 
    Context Method SimpleURL StringHeaders Status StringHeaders (Publisher IO NodeError Buffer) ()
     -> Promise e IO $ Context Method SimpleURL StringHeaders Status StringHeaders b (Publisher IO NodeError Buffer)
  )
   -> io Http2Server
listen' {http} {options} handler = listen http options handler
