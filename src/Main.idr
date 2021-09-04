module Main

import Control.Monad.Error.Interface
import Control.Monad.Maybe
import Data.String

namespace Request

  public export
  record Request a where
    constructor MkRequest
    body : a

  export
  Functor Request where
    map f req = record { body $= f } req

namespace Response

  public export
  data Status = OK | Moved | BadRequest | InternalError

  export
  record Response a where
    constructor MkResponse
    status : Status 
    body : a

  export
  Functor Response where
    map f res = record { body $= f } res

namespace Step

  export
  record Step (a : Type) (b : Type) where
    constructor MkStep
    request : Request a
    response : Response b

  export
  Bifunctor Step where
    bimap f g step = record { request $= map f, response $= map g } step

namespace Handler

  public export
  Handler : (Type -> Type) -> Type -> Type -> Type -> Type -> Type
  Handler m a b c d = Step a b -> m (Step c d)

hId : Applicative m => Handler m a b a b
hId = pure . id

hEcho : Applicative m => Handler m a b a a
hEcho step = pure $ mapSnd (const step.request.body) step

hMapRequest : Applicative m => (i -> o) -> Handler m i a o a
hMapRequest f step = pure $ mapFst f step

hMapResponse : Applicative m => (i -> o) -> Handler m a i a o
hMapResponse f step = pure $ mapSnd f step

hConstRequest : Applicative m => c-> Handler m a b c b
hConstRequest x = hMapRequest $ const x

hConstResponse : Applicative m => c -> Handler m a b a c
hConstResponse x = hMapResponse $ const x

hParseRequest : MonadError e m => (String -> m a) -> Handler m String res a res
hParseRequest parser step = do
  result <- parser step.request.body
  hConstRequest result step

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

