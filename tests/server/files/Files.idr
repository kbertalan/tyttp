module Files

import Data.Buffer
import Control.Monad.Either
import Control.Monad.Maybe
import Control.Monad.Trans
import Node.HTTP.Server
import Node.HTTP.Client
import Node
import System.Directory
import TyTTP.Adapter.Node.HTTP
import TyTTP.Adapter.Node.Static
import TyTTP.HTTP
import TyTTP.HTTP.Producer
import TyTTP.HTTP.Routing
import TyTTP.URL
import TyTTP.URL.Path

sendError :
  Error e
  => HasIO io
  => Status
  -> String
  -> Step me u h1 fn s StringHeaders a b
  -> io $ Step me u h1 fn Status StringHeaders a (Publisher IO e Buffer)
sendError st str step = do
  text str step >>= status st

staticFileError : Error e => HasIO io => FileServingError -> StaticRequest e u -> io $ StaticResponse e u
staticFileError code step = case code of
  StatError e => sendError INTERNAL_SERVER_ERROR ("File error: " <+> message e) step
  NotAFile s => sendError NOT_FOUND ("Could not found file: " <+> s) step

hRouting : Error e
  => String
  -> StaticRequest e String
  -> Promise e IO $ StaticResponse e String
hRouting folder =
    let routingError = sendError NOT_FOUND "Resource could not be found"
        urlError = \err => sendError BAD_REQUEST "URL has invalid format"
    in
      url' urlError :>
        routes' routingError
          [ get $ pattern "/static/*" :> hStatic folder staticFileError
          ]

main : IO ()
main = eitherT putStrLn pure $ do
  Just folder <- currentDir
    | _ => putStrLn "There is no current folder"

  http <- HTTP.require
  server <- HTTP.listen' $ hRouting "\{folder}/"

  defer $ ignore $ http.get "http://localhost:3000/static/run" $ \res => do
      putStrLn res.statusCode
      onData res putStrLn
      server.close

