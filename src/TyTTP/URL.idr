module TyTTP.URL

import Data.List
import Data.String

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
record URL s where
  constructor MkURL
  scheme : Maybe Scheme
  authority : Maybe String
  path : String
  search : s

namespace Simple

  public export
  data URLParserError
    = EmptyString
    | MissingAuthorityOrPath

  export
  parse : String -> Either URLParserError (URL String)
  parse str = scheme (unpack $ trim str) $ MkURL Nothing Nothing "" ""
    where
      search : List Char -> URL String -> Either URLParserError (URL String)
      search [] url = Right url
      search xs url = Right $ { search := pack xs } url

      path : List Char -> URL String -> Either URLParserError (URL String)
      path [] url = Right $ { path := "/" } url
      path xs url =
        let (p, rest) = List.break (== '?') xs
        in search rest $ { path := pack p } url

      authority : List Char -> URL String -> Either URLParserError (URL String)
      authority ('/' :: '/' :: xs) url =
        let (auth, rest) = List.break (== '/') xs
        in path rest $ { authority := Just $ pack auth } url
      authority [] _ = Left MissingAuthorityOrPath
      authority x url = path x url


      scheme : List Char -> URL String -> Either URLParserError (URL String)
      scheme ('h' :: 't' :: 't' :: 'p' :: xs) url = 
        case xs of
           ('s' :: ':' :: ys) => authority ys $ { scheme := Just HTTPS } url
           (':' :: ys) => authority ys $ { scheme := Just HTTP } url
           xs => path ('h' :: 't' :: 't' :: 'p' :: xs) url
      scheme [] _ = Left EmptyString
      scheme xs url = authority xs url

