module Node.TLS.Context

import Data.Buffer.Ext
import Data.Maybe

public export
record Options where
  constructor MkOptions
  ca: List String
  cert: List String
  sigalgs: List String
  ciphers: Maybe String
  clientCertEngine: Maybe String
  crl: List String
  dhparam: Maybe String
  ecdhCurve: Maybe String
  honorCipherOrder: Maybe Bool
  key: List String
  privateKeyEngine: Maybe String
  privateKeyIdentifier: Maybe String
  maxVersion: Maybe String
  minVersion: Maybe String
  passphrase: Maybe String
  pfx: List String
  secureOptions: Maybe Int
  secureProtocol: Maybe String
  sessionIdContext: Maybe String
  ticketKeys: Maybe Buffer
  sessionTimeout: Int

export
defaultOptions : Context.Options
defaultOptions = MkOptions
  { ca = []
  , cert = []
  , sigalgs = []
  , ciphers = Nothing
  , clientCertEngine = Nothing
  , crl = []
  , dhparam = Nothing
  , ecdhCurve = Nothing
  , honorCipherOrder = Nothing
  , key = []
  , privateKeyEngine = Nothing
  , privateKeyIdentifier = Nothing
  , maxVersion = Nothing
  , minVersion = Nothing
  , passphrase = Nothing
  , pfx = []
  , secureOptions = Nothing
  , secureProtocol = Nothing
  , sessionIdContext = Nothing
  , ticketKeys = Nothing
  , sessionTimeout = 300
  }

export
data NodeTLSSecureContextOptions : Type where [external]

%foreign """
  node:lambda:
  ( ca
  , cert
  , sigalgs
  , ciphers
  , clientCertEngine
  , crl
  , dhparam
  , ecdhCurve
  , honorCipherOrder
  , key
  , privateKeyEngine
  , privateKeyIdentifier
  , maxVersion
  , minVersion
  , passphrase
  , pfx
  , secureOptions
  , secureProtocol
  , sessionIdContext
  , ticketKeys
  , sessionTimeout
  ) => ({
    ca: __prim_idris2js_array(ca),
    cert: __prim_idris2js_array(cert),
    sigalgs: sigalgs.length > 0 ? __prim_idris2js_array(sigalgs).join(',') : undefined,
    ciphers: ciphers || undefined,
    clientCertEngine: clientCertEngine || undefined,
    crl: __prim_idris2js_array(crl),
    dhparam: dhparam || undefined,
    ecdhCurve: ecdhCurve || undefined,
    honorCipherOrder: honorCipherOrder == -1 ? undefined : honorCipherOrder != 0,
    key: __prim_idris2js_array(key),
    privateKeyEngine: privateKeyEngine || undefined,
    privateKeyIdentifier: privateKeyIdentifier || undefined,
    maxVersion: maxVersion || undefined,
    minVersion: minVersion || undefined,
    passphrase: passphrase || undefined,
    pfx: __prim_idris2js_array(pfx),
    secureOptions: secureOptions != -1 ? secureOptions : undefined,
    secureProtocol: secureProtocol || undefined,
    sessionIdContext: sessionIdContext || undefined,
    ticketKeys: ticketKeys.length != 0 ? ticketKeys : undefined,
    sessionTimeout
  })
  """
ffi_convertOptions:
  (ca: List String) ->
  (cert: List String) ->
  (sigalgs: List String) ->
  (ciphers: String) ->
  (clientCertEngine: String) ->
  (crl: List String) ->
  (dhparam: String) ->
  (ecdhCurve: String) ->
  (honorCipherOrder: Int) ->
  (key: List String) ->
  (privateKeyEngine: String) ->
  (privateKeyIdentifier: String) ->
  (maxVersion: String) ->
  (minVersion: String) ->
  (passphrase: String) ->
  (pfx: List String) ->
  (secureOptions: Int) ->
  (secureProtocol: String) ->
  (sessionIdContext: String) ->
  (ticketKeys: Buffer) ->
  (sessionTimeout: Int) ->
  NodeTLSSecureContextOptions

export
convertOptions : Context.Options -> NodeTLSSecureContextOptions
convertOptions o = ffi_convertOptions
  o.ca
  o.cert
  o.sigalgs
  (fromMaybe "" o.ciphers)
  (fromMaybe "" o.clientCertEngine)
  o.crl
  (fromMaybe "" o.dhparam)
  (fromMaybe "" o.ecdhCurve)
  (fromMaybe (-1) $ map (\b => if b then 1 else 0) o.honorCipherOrder)
  o.key
  (fromMaybe "" o.privateKeyEngine)
  (fromMaybe "" o.privateKeyIdentifier)
  (fromMaybe "" o.maxVersion)
  (fromMaybe "" o.minVersion)
  (fromMaybe "" o.passphrase)
  o.pfx
  (fromMaybe (-1) o.secureOptions)
  (fromMaybe "" o.secureProtocol)
  (fromMaybe "" o.sessionIdContext)
  (fromMaybe (fromString "") o.ticketKeys)
  o.sessionTimeout

