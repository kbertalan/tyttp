module Main

import Control.Monad.Error.Interface
import Control.Monad.Maybe
import Data.String
import Handler
import Handler.Combinators

parseIntegerMaybeT : (Monad m, Num a, Neg a) => String -> MaybeT m a
parseIntegerMaybeT s = case parseInteger s of
  Nothing => nothing
  Just a  => just a

exampleBasic : Step String String -> IO ()
exampleBasic initialStep = do
  let handlers =
        [ hId
        , hEcho
        , hMapRequest (<+> "-appended") >=> hEcho
        , hEcho >=> hMapResponse (<+> "-response")
        , hConstRequest "const-request" >=> hEcho
        , hEcho >=> hConstResponse "const-response"
        ]
  for_ handlers $ \handler => do
      result <- handler initialStep
      putStrLn result.response.body

exampleRunWithFoldlM : Step String String -> IO ()
exampleRunWithFoldlM initialStep = do
  putStrLn "\nfoldlM\n"

  let collection = [ hConstRequest "init"
                   , hMapRequest (<+> "-req")
                   , hEcho
                   , hMapResponse (<+> "-res")
                   ] 
  result <- foldlM (flip ($)) initialStep collection

  putStrLn result.response.body

Monad m => MonadError Unit (MaybeT m) where
  throwError e = nothing
  catchError ma f = MkMaybeT $ do
    result <- runMaybeT ma
    case result of
        Nothing => runMaybeT $ f ()
        Just a  => pure $ Just a

exampleErrorHandling : Step String String -> IO ()
exampleErrorHandling initialStep = do
  putStrLn $ "\nErrors: " <+> initialStep.request.body <+> "\n"

  let handler = hParseRequest {a=Int} parseIntegerMaybeT >=> hEcho
  result <- runMaybeT $ handler initialStep
  case result of
    Nothing => putStrLn "Error during parsing int"
    Just r  => putStrLn $ show r.response.body

main : IO ()
main = do
  let req = MkRequest "request"
      res = MkResponse OK ""
      step = MkStep req res

  exampleBasic step
  exampleRunWithFoldlM step
  exampleErrorHandling step
  exampleErrorHandling $ MkStep (MkRequest "134") res

