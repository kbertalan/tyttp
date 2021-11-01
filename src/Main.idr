module Main

import Data.Buffer
import Data.List
import Control.Monad.Either
import Control.Monad.Maybe
import Control.Monad.Trans
import Node.Error
import Node.Buffer
import Node.HTTP.Server
import System
import System.Directory
import TyTTP.Adapter.Node.HTTP
import TyTTP.Adapter.Node.Static
import TyTTP.HTTP
import TyTTP.HTTP.Routing as R
import TyTTP.Path
import TyTTP.Routing as R
import TyTTP.URL

sendError : Status -> String -> StaticRequest a -> IO $ StaticResponse a
sendError status str step = do
  let buffer = fromString str
      publisher : Publisher IO NodeError Buffer = singleton buffer

  size <- rawSize buffer

  pure $ record 
    { response.headers = 
      [ ("Content-Type", "text/plain")
      , ("Content-Length", show size)
      ]
    , response.status = status
    , response.body = publisher
    } step

staticFileError : FileServingError -> StaticRequest u -> IO $ StaticResponse u
staticFileError code step = case code of
  StatError e => sendError INTERNAL_SERVER_ERROR ("File error: " <+> e.message) step
  NotAFile s => sendError NOT_FOUND ("Could not found file: " <+> s) step

hRouting : String -> StaticRequest String -> IO $ StaticResponse String
hRouting folder =
    let routingError = sendError NOT_FOUND "Resource could not be found"
        urlError = \err => sendError BAD_REQUEST "URL has invalid format"
    in
      Simple.urlWithHandler urlError :> R.routesWithDefault routingError
          [ R.get $ pattern "/static/*" :> hStatic folder staticFileError 
          , R.post :> sendError INTERNAL_SERVER_ERROR "This is just an example"
          ]

main : IO ()
main = eitherT putStrLn pure $ do
  Just folder <- ((head' <=< tail' <=< tail') <$> getArgs) <|> currentDir
    | Nothing => throwError "Folder could not be located"

  putStrLn "Starting static server in folder: \{folder}"

  http <- HTTP.require
  ignore $ HTTP.listen $ hRouting folder

