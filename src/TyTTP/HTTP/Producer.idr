module TyTTP.HTTP.Producer

import Data.Buffer
import TyTTP
import TyTTP.HTTP.Protocol

unsafeFromString : String -> Buffer
unsafeFromString str = unsafePerformIO $ do
  let size = stringByteLength str
  Just buffer <- newBuffer $ cast size
       | Nothing => assert_total $ idris_crash "could not create new buffer"
  setString buffer 0 str
  pure buffer

export
sendText :
  Applicative m
  => String
  -> Context me u v h1 s StringHeaders a b
  -> m $ Context me u v h1 s StringHeaders a (Publisher IO e Buffer)
sendText str ctx = do
  let stream : Publisher IO e Buffer = Stream.singleton $ unsafeFromString str
  pure $ { response.body := stream
         , response.headers := 
           [ ("Content-Type", "text/plain")
           , ("Content-Length", show $ length str)
           ]
         } ctx

export
status :
  Applicative m
  => Status
  -> Context me u v h1 s h2 a b
  -> m $ Context me u v h1 Status h2 a b
status s ctx = pure $ { response.status := s } ctx

