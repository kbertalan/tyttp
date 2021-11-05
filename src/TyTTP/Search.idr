module TyTTP.Search

import Control.Monad.Either
import Data.List
import Data.List1
import Data.Maybe
import TyTTP
import TyTTP.URL

%default total

namespace Simple

  public export
  SimpleSearch : Type
  SimpleSearch = List (String, String)

  parseString : String -> SimpleSearch
  parseString = parseSkipQuestionMarks . unpack
    where
      parse : List Char -> SimpleSearch
      parse [] = []
      parse xs =
        let sections = splitOn '&' xs
            params = break (== '=') <$> List.filter (not . null) (forget sections)
        in 
          bimap pack (pack . fromMaybe [] . tail') <$> params

      parseSkipQuestionMarks : List Char -> SimpleSearch
      parseSkipQuestionMarks [] = []
      parseSkipQuestionMarks ('?'::xs) = parseSkipQuestionMarks xs
      parseSkipQuestionMarks a@(x::xs) = parse a


  export
  search : Monad m
    => (
      Step me (URL auth pth SimpleSearch) h1 fn st h2 a b
      -> m $ Step me' (URL auth pth SimpleSearch) h1' fn' st' h2' a' b'
    )
    -> Step me (URL auth pth String) h1 fn st h2 a b
    -> m $ Step me' (URL auth pth String) h1' fn' st' h2' a' b'
  search handler step = do
    let src = parseString $ URL.search step.request.url
    result <- handler $ { request.url := { search := src } step.request.url } step
    pure $ { request.url := { search := step.request.url.search } result.request.url } result

