module Main

import Control.Monad.Error.Interface
import Control.Monad.Error.Either
import Data.String
import TyTTP
import TyTTP.Combinators

orThrow : MonadError e m => Maybe a -> e -> m a
orThrow m e = case m of
  Just a  => pure a
  Nothing => throwError e

data Error = ParseError String

parseIntegerM : MonadError Error m => String -> m Int
parseIntegerM s = parseInteger s `orThrow` ParseError ("Could not parse as int: " <+> s)

exampleErrorHandling : Step Method () TyTTP.Request.simpleBody () String String -> IO ()
exampleErrorHandling initialStep = do
  putStrLn $ "\nErrors: " <+> initialStep.request.body <+> "\n"

  let handler = hParseRequest parseIntegerM >=> hEcho {m = EitherT Error IO}
  result <- runEitherT $ handler initialStep

  case result of
    Left  (ParseError e) => putStrLn e
    Right a => putStrLn $ show a.response.body

main : IO ()
main = do
  let res = MkResponse OK () ""
      step = MkStep (MkRequest GET () "request") res

  exampleErrorHandling step
  exampleErrorHandling $ MkStep (MkRequest GET () "134") res

