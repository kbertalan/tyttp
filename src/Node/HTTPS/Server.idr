module Node.HTTPS.Server

import public Node.HTTPS
import public Node.Headers
import public Node.TLS.Context
import public Node.TLS.Server
import public Node.Net.Server

import Data.Buffer.Ext
import TyTTP
import TyTTP.HTTP
import Node.Error
import Node.HTTP.Server

%hide Node.HTTP.Server.Server

public export
Options : Type
Options = Node.HTTP.Server.Options

public export
defaultOptions : HTTPS.Server.Options
defaultOptions = Node.HTTP.Server.defaultOptions

export
data Server : Type where [external]

%foreign """
  node:lambda:
  (https, netServerOptions, tlsServerOptions, tlsContextOptions, secureServerOptions) =>
    https.createServer({
      ...netServerOptions,
      ...tlsServerOptions,
      ...tlsContextOptions,
      ...secureServerOptions
    })
  """
ffi_createServer : HTTPS -> NodeServerOptions -> NodeTLSServerOptions -> NodeTLSSecureContextOptions -> NodeHTTPServerOptions -> PrimIO Server

export
(.createServer) : HasIO io => HTTPS -> Net.Server.Options -> TLS.Server.Options -> TLS.Context.Options -> HTTPS.Server.Options -> io Server
(.createServer) https netServerOptions tlsServerOptions tlsContextOptions httpsOptions = primIO $ ffi_createServer https (convertOptions netServerOptions) (convertOptions tlsServerOptions) (convertOptions tlsContextOptions) (convertOptions httpsOptions)

%foreign "node:lambda: (server, handler) => server.on('request', (req, res) => handler(req)(res)())"
ffi_onRequest : Server -> (IncomingMessage -> ServerResponse -> PrimIO ()) -> PrimIO ()

export
(.onRequest) : HasIO io => Server -> (IncomingMessage -> ServerResponse -> IO()) -> io ()
(.onRequest) server callback = 
  let primCallback = \req => \res => toPrim $ callback req res
  in primIO $ ffi_onRequest server primCallback


%foreign "node:lambda: (server, options) => server.listen(options)"
ffi_listen : Server -> NodeListenOptions -> PrimIO ()

export
(.listen) : HasIO io => Server -> Listen.Options -> io ()
(.listen) server options = primIO $ ffi_listen server $ convertOptions options

%foreign "node:lambda: server => server.close()"
ffi_close : Server -> PrimIO ()

export
(.close) : HasIO io => Server -> io ()
(.close) server = primIO $ ffi_close server

