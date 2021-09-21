module Node

%foreign "node:lambda: (ty, a) => JSON.stringify(a, null, 2)"
ffi_toJsonString : a -> String

export
toJsonString : a -> String
toJsonString a = ffi_toJsonString a


%foreign "node:lambda: (ty, a) => console.log(a)"
ffi_debugJsValue : a -> PrimIO ()

export
debugJsValue : a -> IO ()
debugJsValue a = primIO $ ffi_debugJsValue a

%foreign "node:lambda: cb => setTimeout(() => cb(), 0)"
ffi_defer : PrimIO () -> PrimIO ()

export
defer : IO () -> IO ()
defer action = primIO $ ffi_defer $ toPrim action
