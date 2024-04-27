module TyTTP.HTTP.Producer.JSON

import Data.Buffer.Ext
import TyTTP
import TyTTP.HTTP
import JSON

export
sendJSON :
  Applicative m
  => ToJSON j
  => j
  -> Context me u v h1 s StringHeaders a b
  -> m $ Context me u v h1 s StringHeaders a (Publisher IO e Buffer)
sendJSON j ctx = do
  let bodyJson = encode j
  let stream : Publisher IO e Buffer = Stream.singleton $ fromString $ bodyJson
  pure $ { response.body := stream
         , response.headers :=
           [ ("Content-Type", "application/json")
           , ("Content-Length", show $ length bodyJson)
           ]
         } ctx
