module Node.FS.Stats

import Node.FS
import Node.Error

export
data Stats : Type where [external]

%foreign "node:lambda: (fs,path) => { try { return { h: 1, a1: fs.statSync(path) }; } catch(e) { return { h: 0, a1: e }; } }"
ffi_stat : FS -> String -> PrimIO (Either NodeError Stats)

export
stat : HasIO io => {auto fs : FS} -> String -> io (Either NodeError Stats)
stat path = primIO $ ffi_stat fs path

%foreign "node:lambda: s=>s.isBlockDevice()"
ffi_isBlockDevice : Stats -> Int

export
(.isBlockDevice) : Stats -> Bool
(.isBlockDevice) stats = 0 /= ffi_isBlockDevice stats

%foreign "node:lambda: s=>s.isCharacterDevice() ? 1 : 0"
ffi_isCharacterDevice : Stats -> Int

export
(.isCharacterDevice) : Stats -> Bool
(.isCharacterDevice) stats = 0 /= ffi_isCharacterDevice stats

%foreign "node:lambda: s=>s.isDirectory() ? 1 : 0"
ffi_isDirectory : Stats -> Int

export
(.isDirectory) : Stats -> Bool
(.isDirectory) stats = 0 /= ffi_isDirectory stats

%foreign "node:lambda: s=>s.isFIFO() ? 1 : 0"
ffi_isFIFO : Stats -> Int

export
(.isFIFO) : Stats -> Bool
(.isFIFO) stats = 0 /= ffi_isFIFO stats

%foreign "node:lambda: s=>s.isFile() ? 1 : 0"
ffi_isFile : Stats -> Int

export
(.isFile) : Stats -> Bool
(.isFile) stats = 0 /= ffi_isFile stats

%foreign "node:lambda: s=>s.isSocket() ? 1 : 0"
ffi_isSocket : Stats -> Int

export
(.isSocket) : Stats -> Bool
(.isSocket) stats = 0 /= ffi_isSocket stats

%foreign "node:lambda: s=>s.isSymbolicLink() ? 1 : 0"
ffi_isSymbolicLink : Stats -> Int

export
(.isSymbolicLink) : Stats -> Bool
(.isSymbolicLink) stats = 0 /= ffi_isSymbolicLink stats

export
%foreign "node:lambda: s=>s.size"
(.size) : Stats -> Int

-- TODO complete the rest of stats
