module Files

import Data.Buffer.Ext
import Node.HTTP
import Node.Timers
import System.Directory
import TyTTP.Adapter.Node.HTTP
import TyTTP.Adapter.Node.Static
import TyTTP.HTTP
import TyTTP.URL

sendError :
  Error e
  => HasIO io
  => Status
  -> String
  -> Context me u v h1 s StringHeaders a b
  -> io $ Context me u v h1 Status StringHeaders a (Publisher IO e Buffer)
sendError st str ctx = do
  sendText str ctx >>= status st

routeDef :
  String
  -> StaticRequest String
  -> Promise NodeError IO $ StaticResponse String
routeDef folder =
    let routingError = sendError NOT_FOUND "Resource could not be found"
        urlError = \err => sendError BAD_REQUEST "URL has invalid format"
    in
      parseUrl' urlError :>
        routes' routingError
          [ get $ pattern "/static/*" :> hStatic folder $ flip $ \ctx =>
              \case
                StatError e => sendError INTERNAL_SERVER_ERROR ("File error: " <+> message e) ctx
                NotAFile s => sendError NOT_FOUND ("Could not found file: " <+> s) ctx
          ]

main : IO ()
main = do
  Just folder <- currentDir
    | _ => putStrLn "There is no current folder"

  http <- HTTP.require
  server <- listen' $ routeDef "\{folder}/"

  ignore $ setImmediate $ ignore $ http.get "http://localhost:3000/static/run" defaultOptions $ \res => do
      putStrLn $ show res.statusCode
      res.onData $ putStrLn . show
      server.close

