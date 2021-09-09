module Main

import Handler
import Handler.Combinators

record Stream (m : Type -> Type) (e : Type) (a : Type) where
  constructor MkStream
  onNext: a -> m ()
  onSucceded: () -> m ()
  onFailed: e -> m ()

namespace Node

  namespace Stream

    export
    data Stream : Type where [external]

    export
    data Readable : Type where [external]

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

    %foreign "node:lambda: (ty, r, a) => r.push(a)"
    ffi_readablePush : {0 a: Type} -> Readable -> a -> PrimIO ()

    export
    (.push) : Readable -> a -> IO ()
    (.push) readable a = primIO $ ffi_readablePush readable a

    %foreign "node:lambda: r => r.push(null)"
    ffi_readablePushEnd :  Readable -> PrimIO ()

    export
    (.pushEnd) : Readable -> IO ()
    (.pushEnd) readable = primIO $ ffi_readablePushEnd readable

    %foreign "node:lambda: (ty, r, data, end, error) => {r.on('data', a => data(a)()); r.on('end', () => end()()); r.on('error', e => error(e)());}"
    ffi_subscribeReadable : {0 e: Type } -> Readable -> (String -> PrimIO ()) -> (() -> PrimIO ()) -> (e -> PrimIO ()) -> PrimIO ()

    export
    (.subscribe) : {0 e : Type} -> Readable -> Main.Stream IO e String -> IO ()
    (.subscribe) r (MkStream onNext onSucceded onFailed) = do
      let primData  = \a => toPrim $ onNext a
          primEnd   = \_ => toPrim $ onSucceded ()
          primError = \e => toPrim $ onFailed e

      primIO $ ffi_subscribeReadable r primData primEnd primError


main : IO ()
main = do
  stream <- Node.Stream.require
  readable <- stream.createReadable

  let stream = MkStream
        (\a => putStrLn a)
        (\_ => putStrLn "Stream finished")
        (\e => pure ())

  readable.subscribe {e = String} stream

  readable.push "Stream first element"
  readable.push "Stream second element"
  readable.pushEnd

  putStrLn "Setup success"
