module TyTTP.URL

import Control.Monad.Either
import Data.List
import Data.String
import TyTTP

public export
data Scheme
  = HTTP
  | HTTPS
  | OtherScheme String

namespace Scheme
  export
  parse : String -> Scheme
  parse "http" = HTTP
  parse "https" = HTTPS
  parse str = OtherScheme str

public export
record URL a p s where
  constructor MkURL
  scheme : Maybe Scheme
  authority : Maybe a
  path : p
  search : s

namespace Simple

  public export
  SimpleURL : Type
  SimpleURL = URL String String String

  public export
  data URLParserError
    = EmptyString
    | MissingAuthorityOrPath

  export -- visible for testing
  parse : String -> Either URLParserError SimpleURL
  parse str = scheme (unpack $ trim str) $ MkURL Nothing Nothing "" ""
    where
      search : List Char -> SimpleURL -> Either URLParserError SimpleURL
      search [] url = Right url
      search xs url = Right $ { search := pack xs } url

      path : List Char -> SimpleURL -> Either URLParserError SimpleURL
      path [] url = Right $ { path := "/" } url
      path xs url =
        let (p, rest) = List.break (== '?') xs
        in search rest $ { path := pack p } url

      authority : List Char -> SimpleURL -> Either URLParserError SimpleURL
      authority ('/' :: '/' :: xs) url =
        let (auth, rest) = List.break (== '/') xs
        in path rest $ { authority := Just $ pack auth } url
      authority [] _ = Left MissingAuthorityOrPath
      authority x url = path x url

      scheme : List Char -> SimpleURL -> Either URLParserError SimpleURL
      scheme ('h' :: 't' :: 't' :: 'p' :: xs) url = 
        case xs of
           ('s' :: ':' :: ys) => authority ys $ { scheme := Just HTTPS } url
           (':' :: ys) => authority ys $ { scheme := Just HTTP } url
           xs => path ('h' :: 't' :: 't' :: 'p' :: xs) url
      scheme [] _ = Left EmptyString
      scheme xs url = authority xs url

  export
  url : MonadError URLParserError m
    => (
      Step me SimpleURL h1 fn s h2 a b
      -> m $ Step me' SimpleURL h1' fn' s' h2' a' b'
    )
    -> Step me String h1 fn s h2 a b
    -> m $ Step me' String h1' fn' s' h2' a' b'
  url handler step = case parse step.request.url of
    Right u => do
      result <- handler $ { request.url := u } step
      pure $ { request.url := step.request.url } result
    Left err => throwError err

  export
  url' : Monad m
    => (
      URLParserError
      -> Step me String h1 fn s h2 a b
      -> m $ Step me' String h1' fn' s' h2' a' b'
    )
    -> (
      Step me SimpleURL h1 fn s h2 a b
      -> EitherT URLParserError m $ Step me' SimpleURL h1' fn' s' h2' a' b'
    )
    -> Step me String h1 fn s h2 a b
    -> m $ Step me' String h1' fn' s' h2' a' b'
  url' errHandler handler step = do
    Right result <- runEitherT $ Simple.url handler step
      | Left err => errHandler err step
    pure result


