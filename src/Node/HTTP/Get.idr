module Node.HTTP.Get

import Node.HTTP

namespace Client
  
  export
  data ClientRequest : Type where [external]

  export
  data IncomingMessage : Type where [external]

  export
  %foreign "node:lambda: res => res.headers"
  (.headers) : Client.IncomingMessage -> Headers


%foreign "node:lambda: (http, url, cb) => http.get(url, (res) => { cb(res)() })"
ffi_get : HTTP -> String -> (Client.IncomingMessage -> PrimIO ()) -> PrimIO ClientRequest

export
(.get) : HTTP -> String -> (Client.IncomingMessage -> IO ()) -> IO ClientRequest
(.get) http url cb = primIO $ ffi_get http url $ \res => toPrim $ cb res
