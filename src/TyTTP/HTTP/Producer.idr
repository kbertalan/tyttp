module TyTTP.HTTP.Producer

import Data.Buffer.Ext
import TyTTP
import TyTTP.HTTP

export
text :
  Applicative m
  => String
  -> Step me u h1 s StringHeaders a b
  -> m $ Step me u h1 s StringHeaders a (Publisher IO e Buffer)
text str step = do
  let stream : Publisher IO e Buffer = Stream.singleton $ fromString str
  pure $ { response.body := stream
         , response.headers := 
           [ ("Content-Type", "text/plain")
           , ("Content-Length", show $ length str)
           ]
         } step

export
status :
  Applicative m
  => Status
  -> Step me u h1 s h2 a b
  -> m $ Step me u h1 Status h2 a b
status s step = pure $ { response.status := s } step

