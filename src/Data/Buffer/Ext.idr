module Data.Buffer.Ext

import Data.Buffer

%foreign "scheme:blodwen-new-buffer"
         "RefC:newBuffer"
         "node:lambda:s=>Buffer.alloc(s)"
prim__newBuffer : Int -> PrimIO Buffer

export
newBuffer : HasIO io => Nat -> io Buffer
newBuffer size = primIO (Ext.prim__newBuffer $ cast size)

