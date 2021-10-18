module Node.FS.Stream

import Node.FS

data ReadStream : Type where [external]

%foreign "node:lambda: (fs,path)=>fs.createReadStream(path)"
ffi_createReadStream : FS -> String -> PrimIO ReadStream

export
createReadStream : HasIO io => { auto fs : FS } -> String -> io ReadStream
createReadStream path = primIO $ ffi_createReadStream fs path


%foreign "node:lambda: (ty, req, data) => { req.on('data', a => data(a)()) }"
ffi_onData : ReadStream -> (a -> PrimIO ()) -> PrimIO ()

export
(.onData) : HasIO io => ReadStream -> (a -> IO ()) -> io ()
(.onData) req cb = primIO $ ffi_onData req $ \a => toPrim $ cb a

%foreign "node:lambda: (req, end) => { req.on('end', () => end()()) }"
ffi_onEnd : ReadStream -> (() -> PrimIO ()) -> PrimIO ()

export
(.onEnd) : HasIO io => ReadStream -> (() -> IO ()) -> io ()
(.onEnd) req cb = primIO $ ffi_onEnd req $ \_ => toPrim $ cb ()

%foreign "node:lambda: (ty, req, error) => { req.on('error', e => error(e)()) }"
ffi_onError : ReadStream -> (e -> PrimIO ()) -> PrimIO ()

export
(.onError) : HasIO io => ReadStream -> (e -> IO ()) -> io ()
(.onError) req cb = primIO $ ffi_onError req $ \e => toPrim $ cb e

