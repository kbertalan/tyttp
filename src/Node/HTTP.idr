module Node.HTTP

export
data HTTP : Type where [external]

%foreign "node:lambda: () => require('http')"
ffi_require : () -> PrimIO HTTP

export
require : HasIO io => io HTTP
require = primIO $ ffi_require ()

