module TyTTP.URL.Search

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
      Context me (URL auth pth SimpleSearch) h1 st h2 a b
      -> m $ Context me' (URL auth pth SimpleSearch) h1' st' h2' a' b'
    )
    -> Context me (URL auth pth String) h1 st h2 a b
    -> m $ Context me' (URL auth pth String) h1' st' h2' a' b'
  search handler ctx = do
    let src = parseString $ URL.search ctx.request.url
    result <- handler $ { request.url := { search := src } ctx.request.url } ctx
    pure $ { request.url := { search := ctx.request.url.search } result.request.url } result

