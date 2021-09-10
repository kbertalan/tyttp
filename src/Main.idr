module Main

import Data.IORef
import Handler
import Handler.Combinators

record Subscriber (m : Type -> Type) (e : Type) (a : Type) where
  constructor MkSubscriber
  onNext: a -> m ()
  onSucceded: () -> m ()
  onFailed: e -> m ()

data Publisher : (Type -> Type) -> Type -> Type -> Type where
  MkPublisher : (Subscriber m e a -> m ()) -> Publisher m e a

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
    (.subscribe) : {0 e : Type} -> Readable -> Subscriber IO e String -> IO ()
    (.subscribe) r (MkSubscriber onNext onSucceded onFailed) = do
      let primData  = \a => toPrim $ onNext a
          primEnd   = \_ => toPrim $ onSucceded ()
          primError = \e => toPrim $ onFailed e

      primIO $ ffi_subscribeReadable r primData primEnd primError

hConsumeBody : Handler IO (Publisher IO String String) a (Publisher IO String String) a
hConsumeBody step = do
  cache <- newIORef ""
  let publisher = MkPublisher $ \s => do
    let subscriber = MkSubscriber
                       (\a => modifyIORef cache (<+> a))
                       (\_ => readIORef cache >>= s.onNext >> s.onSucceded ())
                       (\e => s.onFailed e)

    MkPublisher subscribe <- pure step.request.body
    subscribe subscriber

  hConstRequest publisher step


main : IO ()
main = do
  stream <- Node.Stream.require
  readable <- stream.createReadable

  let publisher = MkPublisher $ \s => readable.subscribe { e = String } s

  let req = MkRequest publisher
      res = MkResponse OK ()
      handler = hConsumeBody >=> hEcho

  result <- handler $ MkStep req res

  readable.push "Stream first element\n"
  readable.push "Stream second element\n"
  readable.pushEnd

  let actions = MkSubscriber
        (\a => putStrLn a)
        (\_ => putStrLn "Stream finished")
        (\e => putStrLn $ "Error happened: " <+> e)
  MkPublisher subscribe <- pure result.response.body
  subscribe actions
