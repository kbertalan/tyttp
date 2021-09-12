module Main

import Data.String
import Data.IORef
import Handler
import Handler.Combinators
import Stream
import Node.Stream.Readable
import Node.Stream

hConsumeBody : Handler IO (Publisher IO String String) a (Publisher IO String String) a
hConsumeBody step = do
  cache <- newIORef ""
  let publisher = MkPublisher $ \s => do
    let subscriber = MkSubscriber
          (\a => modifyIORef cache (<+> a))
          (\_ => readIORef cache >>= s.onNext >>= s.onSucceded)
          (\e => s.onFailed e)

    step.request.body.subscribe subscriber

  hConstRequest publisher step

main : IO ()
main = do
  stream <- Node.Stream.require
  readable <- stream.createReadable

  let publishFromReadable = MkPublisher $ \s => readable.subscribe { e = String } s

  let req = MkRequest publishFromReadable
      res = MkResponse OK ()
      handler = hConsumeBody >=> hEcho

  result <- handler $ MkStep req res

  let actions = MkSubscriber
        (\a => putStrLn a)
        (\_ => putStrLn "Stream finished")
        (\e => putStrLn $ "Error happened: " <+> e)

  result.response.body.subscribe actions

  readable.push "Stream first element\n"
  readable.push "Stream second element\n"
  readable.pushEnd
