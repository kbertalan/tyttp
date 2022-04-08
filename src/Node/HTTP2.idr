module Node.HTTP2

export
data HTTP2 : Type where [external]

%foreign "node:lambda: () => require('http2')"
ffi_require : () -> PrimIO HTTP2

export
require : HasIO io => io HTTP2
require = primIO $ ffi_require ()

