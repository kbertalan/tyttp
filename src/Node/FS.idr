module Node.FS

export
data FS : Type where [external]

%foreign "node:lambda: () => require('fs')"
ffi_require : PrimIO FS

export
require : HasIO io => io FS
require = primIO ffi_require

