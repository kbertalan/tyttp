module Node.HTTPS

export
data HTTPS : Type where [external]

%foreign "node:lambda: () => require('https')"
ffi_require : () -> PrimIO HTTPS

export
require : HasIO io => io HTTPS
require = primIO $ ffi_require ()

