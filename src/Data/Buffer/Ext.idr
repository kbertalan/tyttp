module Data.Buffer.Ext

import public Data.Buffer

%foreign "scheme:blodwen-new-buffer"
         "RefC:newBuffer"
         "node:lambda:s=>Buffer.alloc(s)"
prim__newBuffer : Int -> PrimIO Buffer

export
newBuffer : HasIO io => Nat -> io Buffer
newBuffer size = primIO (Ext.prim__newBuffer $ cast size)

%foreign "scheme:string->utf8"
         "node:lambda: s=>Buffer.from(s, 'utf8')"
ffi_BufferFromStringUTF8 : String -> Buffer

export
implementation FromString Buffer where
  fromString = ffi_BufferFromStringUTF8

%foreign "scheme:utf8->string"
         "node:lambda: b=>b.toString('utf8')"
ffi_BufferToStringUTF8 : Buffer -> String

export
implementation Show Buffer where
  show = ffi_BufferToStringUTF8

