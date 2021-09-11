module Node.Stream.Readable

import Stream

export
data Readable : Type where [external]

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

