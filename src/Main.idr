module Main

import Data.IORef
import Handler
import Handler.Combinators
import Node.Stream
import Node.Stream.Readable
import Stream

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

streamMonad : IO ()
streamMonad = do
  let subscriber = MkSubscriber
                    { onNext = \a => putStrLn $ "Next: " <+> a }
                    { onSucceded = \_ => putStrLn $ "Success" }
                    { onFailed = \e => putStrLn $ "Error: " <+> e }
      duplicate = \a => MkPublisher $ \s => s.onNext a >> s.onNext a >>= s.onSucceded

      initial : Publisher IO String String
      initial = MkPublisher $ \s => s.onNext "a" >> s.onNext "b" >>= s.onSucceded

      chain : Publisher IO String String
      chain = do
        a <- initial
        duplicate a

      failingDuplicate : String -> String -> Publisher IO String String
      failingDuplicate a e = MkPublisher $ \s => s.onNext a >> s.onFailed e

      failingChain : Publisher IO String String
      failingChain = do
        a <- initial
        failingDuplicate a "second is error"

  putStrLn "successful chain:"
  subscribe chain subscriber

  putStrLn ""

  putStrLn "failing chain:"
  subscribe failingChain subscriber

  putStrLn ""
  putStrLn "End of monadic publisher\n"

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

  streamMonad
