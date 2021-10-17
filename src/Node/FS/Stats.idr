module Node.FS.Stats

import Node.FS

export
data Stats : Type where [external]

%foreign "node:lambda: (fs,path) => fs.statSync(path)"
ffi_stat : FS -> String -> PrimIO Stats

export
stat : {auto fs : FS} -> String -> IO Stats
stat path = primIO $ ffi_stat fs path

%foreign "node:lambda: s=>s.isBlockDevice()"
ffi_isBlockDevice : Stats -> Int

export
(.isBlockDevice) : Stats -> Bool
(.isBlockDevice) stats = 0 /= ffi_isBlockDevice stats

%foreign "node:lambda: s=>s.isCharacterDevice()"
ffi_isCharacterDevice : Stats -> Int

export
(.isCharacterDevice) : Stats -> Bool
(.isCharacterDevice) stats = 0 /= ffi_isCharacterDevice stats

%foreign "node:lambda: s=>s.isDirectory()"
ffi_isDirectory : Stats -> Int

export
(.isDirectory) : Stats -> Bool
(.isDirectory) stats = 0 /= ffi_isDirectory stats

%foreign "node:lambda: s=>s.isFIFO()"
ffi_isFIFO : Stats -> Int

export
(.isFIFO) : Stats -> Bool
(.isFIFO) stats = 0 /= ffi_isFIFO stats

%foreign "node:lambda: s=>s.isFile()"
ffi_isFile : Stats -> Int

export
(.isFile) : Stats -> Bool
(.isFile) stats = 0 /= ffi_isFile stats

%foreign "node:lambda: s=>s.isSocket()"
ffi_isSocket : Stats -> Int

export
(.isSocket) : Stats -> Bool
(.isSocket) stats = 0 /= ffi_isSocket stats

%foreign "node:lambda: s=>s.isSymbolicLink()"
ffi_isSymbolicLink : Stats -> Int

export
(.isSymbolicLink) : Stats -> Bool
(.isSymbolicLink) stats = 0 /= ffi_isSymbolicLink stats

-- TODO complete the rest of stats
