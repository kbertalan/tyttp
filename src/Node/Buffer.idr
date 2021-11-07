module Node.Buffer

import Data.Buffer

%foreign "node:lambda: s=>Buffer.from(s)"
ffi_BufferFromStringUTF8 : String -> Buffer

export
FromString Buffer where
  fromString = ffi_BufferFromStringUTF8

export
%foreign "node:lambda: b=>b.toString()"
toStringUTF8 : Buffer -> String
