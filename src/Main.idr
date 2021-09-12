module Main

import Stream

main : IO ()
main = do
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

