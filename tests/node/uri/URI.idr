module URI

import Node.URI

test : (fn : String -> Either URIError String) -> String -> String
test fn input = case fn input of
  Left (MkURIError message) => message
  Right result => result

main : IO ()
main = do
  -- examples from: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURI
  putStrLn $ test decodeURI "https://developer.mozilla.org/ru/docs/JavaScript_%D1%88%D0%B5%D0%BB%D0%BB%D1%8B"
  putStrLn $ test decodeURI "%E0%A4%A"

  -- examples from: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent
  putStrLn $ test decodeURIComponent "JavaScript_%D1%88%D0%B5%D0%BB%D0%BB%D1%8B"
  putStrLn $ test decodeURIComponent "%E0%A4%A"
  putStrLn $ test decodeURIComponent "search+query%20%28correct%29"

  -- examples from: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI
  putStrLn $ test decodeURI $ test encodeURI "https://mozilla.org/?x=шеллы"

  let set1 = ";,/?:@&=+$#"
      set2 = "-_.!~*'()"
      set3 = "ABC abc 123"

  putStrLn $ test encodeURI set1
  putStrLn $ test encodeURI set2
  putStrLn $ test encodeURI set3

  putStrLn $ test encodeURIComponent set1
  putStrLn $ test encodeURIComponent set2
  putStrLn $ test encodeURIComponent set3

--  putStrLn $ test encodeURI "\uD800\uDFFF"
--  putStrLn $ test encodeURI "\uD800"
--  putStrLn $ test encodeURI "\uDFFF"

  -- examples from: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent
  putStrLn $ test encodeURIComponent "test?"
  putStrLn $ test encodeURIComponent "шеллы"
