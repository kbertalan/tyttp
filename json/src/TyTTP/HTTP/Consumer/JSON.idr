module TyTTP.HTTP.Consumer.JSON

import Data.Buffer.Ext
import JSON
import TyTTP.HTTP.Consumer

%hide JSON.Parser.JSON

export
data JSON : Type where

export
implementation Accept JSON where
  contentType _ = [ "application/json" ]

export
implementation FromJSON a => Consumer a JSON where
  consumeRaw _ ct raw = 
    mapFst show $ decode $ show raw
