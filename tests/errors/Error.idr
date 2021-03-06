module Error

import Control.Monad.Error.Interface
import Control.Monad.Error.Either
import Data.String
import TyTTP

orThrow : MonadError e m => Maybe a -> e -> m a
orThrow m e = case m of
  Just a  => pure a
  Nothing => throwError e

data Error = ParseError String

parseIntegerM : MonadError Error m => String -> m Int
parseIntegerM s = parseInteger s `orThrow` ParseError ("Could not parse as int: " <+> s)

hParseRequest : MonadError e m
  => (String -> m a')
  -> Step me h1 u Request.simpleBody s h2 String b
  -> m $ Step me h1 u Request.simpleBody s h2 a' b
hParseRequest parser step = do
  result <- parser step.request.body
  pure $ { response.body := result } step

exampleErrorHandling : Step () String () Request.simpleBody () () String String -> IO ()
exampleErrorHandling initialStep = do
  putStrLn $ "\nErrors: " <+> initialStep.request.body <+> "\n"

  let handler = hParseRequest parseIntegerM >=> hEcho {m = EitherT Error IO}
  result <- runEitherT $ handler initialStep

  case result of
    Left  (ParseError e) => putStrLn e
    Right a => putStrLn $ show a.response.body

main : IO ()
main = do
  let res = MkResponse () () ""
      step = MkStep (MkRequest () "/" () "request") res

  exampleErrorHandling step
  exampleErrorHandling $ MkStep (MkRequest () "/" () "134") res

