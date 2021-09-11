module Node.Stream

import Node.Stream.Readable

export
data Stream : Type where [external]

%foreign "node:lambda: () => require('stream')"
ffi_require : () -> PrimIO Node.Stream.Stream

export
require : IO Node.Stream.Stream
require = primIO $ ffi_require ()

%foreign "node:lambda: stream => new stream.Readable({read: () => {}})"
ffi_createReadable : Node.Stream.Stream -> PrimIO Readable

export
(.createReadable) : Node.Stream.Stream -> IO Readable
(.createReadable) s = primIO $ ffi_createReadable s

