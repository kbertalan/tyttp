module TyTTP.URL.Definition

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

  export
  Show Scheme where
    show s = case s of
      HTTP => "http"
      HTTPS => "https"
      OtherScheme str => str

public export
record URL a p s where
  constructor MkURL
  scheme : Maybe Scheme
  authority : Maybe a
  path : p
  search : s

