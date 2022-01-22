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
  -> Context me u h1 s StringHeaders a b
  -> io $ Context me u h1 Status StringHeaders a (Publisher IO e Buffer)
sendError st str ctx = do
  text str ctx >>= status st

routeDef : Error e
  => String
  -> StaticRequest e String
  -> Promise e IO $ StaticResponse e String
routeDef folder =
    let routingError = sendError NOT_FOUND "Resource could not be found"
        urlError = \err => sendError BAD_REQUEST "URL has invalid format"
    in
      parseUrl' urlError :>
        routes' routingError
          [ get $ path "/static/*" :> hStatic folder $ flip $ \ctx =>
              \case
                StatError e => sendError INTERNAL_SERVER_ERROR ("File error: " <+> message e) ctx
                NotAFile s => sendError NOT_FOUND ("Could not found file: " <+> s) ctx
          ]

main : IO ()
main = eitherT putStrLn pure $ do
  Just folder <- currentDir
    | _ => putStrLn "There is no current folder"

  http <- HTTP.require
  server <- HTTP.listen' $ routeDef "\{folder}/"

  defer $ ignore $ http.get "http://localhost:3000/static/run" $ \res => do
      putStrLn res.statusCode
      onData res putStrLn
      server.close

