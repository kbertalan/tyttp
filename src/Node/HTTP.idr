module Node.HTTP

export
data HTTP : Type where [external]

%foreign "node:lambda: () => require('http')"
ffi_require : () -> PrimIO HTTP

export
require : HasIO io => io HTTP
require = primIO $ ffi_require ()

namespace Headers

  export
  data Headers : Type where [external]

  %foreign "node:lambda: () => { return {}; }"
  ffi_empty : () -> PrimIO Headers

  export
  empty : HasIO io => io Headers
  empty = primIO $ ffi_empty ()

  %foreign "node:lambda: (name, value) => { const headers = {}; headers[name] = value; return headers; }"
  ffi_singleton : String -> String -> Headers

  export
  singleton : String -> String -> Headers
  singleton name value = ffi_singleton name value

  %foreign "node:lambda: (headers, name, value) => { headers[name] = value; return headers; }"
  ffi_setHeader : Headers -> String -> String -> PrimIO Headers

  export
  (.setHeader) : HasIO io => Headers -> String -> String -> io Headers
  (.setHeader) headers name value = primIO $ ffi_setHeader headers name value

  %foreign "node:lambda: (headers) => __prim_js2idris_array(Object.entries(headers).map(([k,v]) => { return {h:0,a1:k,a2:v}}))"
  export
  (.asList) : Headers -> List (String, String)

