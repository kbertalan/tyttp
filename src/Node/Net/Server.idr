module Node.Net.Server

import Data.Maybe

public export
record Options where
  constructor MkOptions
  allowHalfOpen : Bool
  pauseOnConnect : Bool
  noDelay : Bool
  keepAlive : Bool
  keepAliveInitialDelay : Int

export
defaultOptions : Net.Server.Options
defaultOptions = MkOptions
  { allowHalfOpen = False
  , pauseOnConnect = False
  , noDelay = False
  , keepAlive = False
  , keepAliveInitialDelay = 0
  }

export
data NodeServerOptions : Type where [external]

%foreign """
  node:lambda:
  ( allowHalfOpen
  , pauseOnConnect
  , noDelay
  , keepAlive
  , keepAliveInitialDelay
  ) => ({
    allowHalfOpen: allowHalfOpen != 0,
    pauseOnConnect: pauseOnConnect != 0,
    noDelay: noDelay != 0,
    keepAlive: keepAlive != 0,
    keepAliveInitialDelay
  })
  """
ffi_convertOptions :
  (allowHalfOpen : Int) ->
  (pauseOnConnect : Int) ->
  (noDelay : Int) ->
  (keepAlive : Int) ->
  (keepAliveInitialDelay : Int) ->
  NodeServerOptions

export
convertOptions : Net.Server.Options -> NodeServerOptions
convertOptions o = ffi_convertOptions
  (if o.allowHalfOpen then 1 else 0)
  (if o.pauseOnConnect then 1 else 0)
  (if o.noDelay then 1 else 0)
  (if o.keepAlive then 1 else 0)
  o.keepAliveInitialDelay

namespace Listen

  public export
  record Options where
    constructor MkOptions
    port : Maybe Int
    host : Maybe String
    path : Maybe String
    backlog : Maybe Int
    exclusive : Bool
    readableAll : Bool
    writableAll : Bool
    ipv6Only : Bool
    -- TODO signal <AbortSignal> An AbortSignal that may be used to close a listening server.

  export
  defaultOptions : Listen.Options
  defaultOptions = MkOptions
    { port = Nothing
    , host = Nothing
    , path = Nothing
    , backlog = Nothing
    , exclusive = False
    , readableAll = False
    , writableAll = False
    , ipv6Only = False
    }

  export
  data NodeListenOptions : Type where [external]

  %foreign """
    node:lambda:
    ( port
    , host
    , path
    , backlog
    , exclusive
    , readableAll
    , writableAll
    , ipv6Only
    ) => ({
      port: port != -1 ? port : undefined,
      host: host || undefined,
      path: path || undefined,
    })
    """
  ffi_convertOptions :
    (port : Int) ->
    (host : String) ->
    (path : String) ->
    (backlog : Int) ->
    (exclusive : Int) ->
    (readableAll : Int) ->
    (writableAll : Int) ->
    (ipv6Only : Int) ->
    NodeListenOptions

  export
  convertOptions : Listen.Options -> NodeListenOptions
  convertOptions o = ffi_convertOptions
    (fromMaybe (-1) o.port)
    (fromMaybe "" o.host)
    (fromMaybe "" o.path)
    (fromMaybe (-1) o.backlog)
    (if o.exclusive then 1 else 0)
    (if o.readableAll then 1 else 0)
    (if o.writableAll then 1 else 0)
    (if o.ipv6Only then 1 else 0)

