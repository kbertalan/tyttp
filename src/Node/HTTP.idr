module Node.HTTP

export
data HTTP : Type where [external]

%foreign "node:lambda: () => require('http')"
ffi_require : () -> PrimIO HTTP

export
require : IO HTTP
require = primIO $ ffi_require ()

namespace Headers

  export
  data Headers : Type where [external]

  %foreign "node:lambda: () => { return {}; }"
  ffi_empty : () -> Headers

  export
  empty : Headers
  empty = ffi_empty ()

  %foreign "node:lambda: (name, value) => { const headers = {}; headers[name] = value; return headers; }"
  ffi_singleton : String -> String -> Headers

  export
  singleton : String -> String -> Headers
  singleton name value = ffi_singleton name value

  %foreign "node:lambda: (headers, name, value) => { headers[name] = value; return headers; }"
  ffi_setHeader : Headers -> String -> String -> PrimIO Headers

  export
  (.setHeader) : Headers -> String -> String -> IO Headers
  (.setHeader) headers name value = primIO $ ffi_setHeader headers name value

