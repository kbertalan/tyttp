module Node.Buffer

import Data.Buffer

%foreign "node:lambda: s=>Buffer.from(s)"
ffi_BufferFromString : String -> Buffer

export
FromString Buffer where
  fromString = ffi_BufferFromString

