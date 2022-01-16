module Main

import Data.Buffer
import Data.Buffer.Ext
import Data.List
import Data.List.Quantifiers
import Control.Monad.Either
import Control.Monad.Maybe
import Control.Monad.Trans
import Generics.Derive
import JSON
import Node.HTTP.Server
import System
import System.Directory
import TyTTP.Adapter.Node.HTTP
import TyTTP.Adapter.Node.Static
import TyTTP.Adapter.Node.URI
import TyTTP.HTTP
import TyTTP.HTTP.Combinators
import TyTTP.HTTP.Consumer
import TyTTP.HTTP.Consumer.JSON
import TyTTP.HTTP.Producer
import TyTTP.HTTP.Routing
import TyTTP.URL
import TyTTP.URL.Path
import TyTTP.URL.Search

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

hQuery : Error e
  => Monad m
  => (src -> String)
  -> Step me (URL auth pth src) h1 fn s StringHeaders a ()
  -> m $ Step me (URL auth pth src) h1 fn Status StringHeaders a (Publisher IO e Buffer)
hQuery toString step = do
  let query = toString step.request.url.search
  text query step >>= status OK

%language ElabReflection

record Example where
  constructor MkExample
  field : String
  opt : Maybe Int

%runElab derive "Example" [Generic, Meta, Show, Eq, RecordFromJSON]

hReturnExample : Error e
  => HasIO m
  => Step me u h1 Request.simpleBody s StringHeaders Example ()
  -> m $ Step me u h1 Request.simpleBody Status StringHeaders Example (Publisher IO e Buffer)
hReturnExample step = do
  let payload = show step.request.body
  text payload step >>= status OK

hRouting : Error e
  => String
  -> StaticRequest e String
  -> Promise e IO $ StaticResponse e String
hRouting folder =
    let routingError = sendError NOT_FOUND "Resource could not be found"
        urlError = \err => sendError BAD_REQUEST "URL has invalid format"
        uriError = sendError BAD_REQUEST "URI decode has failed"
        parseError = \s => sendError BAD_REQUEST "Content cannot be parsed: \{s.request.body}" s
    in
      uri' uriError :> url' urlError :> routes' routingError
          [ get $ pattern "/static/*" :> hStatic folder staticFileError
          , get $ pattern "/query" :>> hQuery id
          , get $ pattern "/parsed" :> Simple.search :> hQuery show
          , post $ pattern "/json" $ consumes' [JSON] parseError :> hReturnExample
          ]


main : IO ()
main = eitherT putStrLn pure $ do
  Just folder <- ((head' <=< tail' <=< tail') <$> getArgs) <|> currentDir
    | Nothing => throwError "Folder could not be located"

  putStrLn "Starting static server in folder: \{folder}"

  http <- HTTP.require
  ignore $ HTTP.listen' $ hRouting folder

