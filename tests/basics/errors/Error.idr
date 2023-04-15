module Error

import Control.Monad.Error.Interface
import Control.Monad.Error.Either
import Data.String
import TyTTP
import TyTTP.HTTP

orThrow : MonadError e m => Maybe a -> e -> m a
orThrow m e = case m of
  Just a  => pure a
  Nothing => throwError e

data Error = ParseError String

parseIntegerM : MonadError Error m => String -> m Int
parseIntegerM s = parseInteger s `orThrow` ParseError ("Could not parse as int: " <+> s)

hParseRequest : MonadError e m
  => (String -> m a')
  -> Context me u v h1 s h2 String b
  -> m $ Context me u v h1 s h2 a' b
hParseRequest parser ctx = do
  result <- parser ctx.request.body
  pure $ { request.body := result } ctx

hEcho : Monad m
  => Context me u v h1 s h2 a b
  -> m $ Context me u v h1 s h2 a a
hEcho ctx = pure $ { response.body := ctx.request.body } ctx

exampleErrorHandling : Context Method String Version () String () String String -> IO ()
exampleErrorHandling ctx = do
  putStrLn $ "\nErrors: " <+> ctx.request.body <+> "\n"

  let handler = hParseRequest parseIntegerM >=> hEcho {m = EitherT Error IO}
  result <- runEitherT $ handler ctx

  case result of
    Left  (ParseError e) => putStrLn e
    Right a => putStrLn $ show a.response.body

main : IO ()
main = do
  let res = MkResponse "" () ""
      ctx = MkContext (MkRequest GET "/" Version_1_1 () "request") res

  exampleErrorHandling ctx
  exampleErrorHandling $ MkContext (MkRequest GET "/" Version_1_1 () "134") res

