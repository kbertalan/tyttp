module Node.TLS.Server

import Data.Buffer.Ext
import Data.Maybe

public export
record Options where
  constructor MkOptions
  -- ALPNProtocols: TODO
  clientCertEngine: Maybe String
  enableTrace: Bool
  handshakeTimeout: Int
  rejectUnauthorized: Bool
  requestCert: Bool
  sessionTimeout: Int
  -- SNICallback(servername, callback): TODO
  ticketKeys: Maybe Buffer
  -- pskCallback: TODO
  pskIdentityHint: Maybe String

export
defaultOptions : TLS.Server.Options
defaultOptions = MkOptions
  { clientCertEngine = Nothing
  , enableTrace = False
  , handshakeTimeout = 120000
  , rejectUnauthorized = True
  , requestCert = False
  , sessionTimeout = 300
  , ticketKeys = Nothing
  , pskIdentityHint = Nothing
  }

export
data NodeTLSServerOptions : Type where [external]

%foreign """
  node:lambda:
  ( clientCertEngine
  , enableTrace
  , handshakeTimeout
  , rejectUnauthorized
  , requestCert
  , sessionTimeout
  , ticketKeys
  , pskIdentityHint
  ) => ({
    clientCertEngine: clientCertEngine || undefined,
    enableTrace: enableTrace != 0,
    handshakeTimeout,
    rejectUnauthorized: rejectUnauthorized != 0,
    requestCert: requestCert != 0
    sessionTimeout,
    ticketKeys: ticketKeys.length ? ticketKeys : undefined,
    pskIdentityHint: pskIdentityHint || undefined
  })
  """
ffi_convertOptions : 
  (clientCertEngine: String) ->
  (enableTrace: Int) ->
  (handshakeTimeout: Int) ->
  (rejectUnauthorized: Int) ->
  (requestCert: Int) ->
  (sessionTimeout: Int) ->
  (ticketKeys: Buffer) ->
  (pskIdentityHint: String) ->
  NodeTLSServerOptions

export
convertOptions : TLS.Server.Options -> NodeTLSServerOptions
convertOptions o = ffi_convertOptions
  (fromMaybe "" o.clientCertEngine)
  (if o.enableTrace then 1 else 0)
  o.handshakeTimeout
  (if o.rejectUnauthorized then 1 else 0)
  (if o.requestCert then 1 else 0)
  o.sessionTimeout
  (fromMaybe (fromString "") o.ticketKeys)
  (fromMaybe "" o.pskIdentityHint)
