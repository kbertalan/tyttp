module TyTTP.HTTP.Producer

import Data.Buffer.Ext
import TyTTP
import TyTTP.HTTP

export
text :
  Applicative m
  => String
  -> Context me u v h1 s StringHeaders a b
  -> m $ Context me u v h1 s StringHeaders a (Publisher IO e Buffer)
text str ctx = do
  let stream : Publisher IO e Buffer = Stream.singleton $ fromString str
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

