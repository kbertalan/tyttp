module Buffer

import Data.Buffer
import Data.Buffer.Ext

main : IO ()
main = do
  let strings =
    [ "árvíztűrő tükörfúrógép"
    , "明日の待ち合わせは、初めて訪れる場所です。あなたは事前に何をしますか？"
    , """
      А а 	Б б 	В в 	Г г 	Д д 	Е е 	Ё ё 	Ж ж 	З з 	И и 	Й й
      К к 	Л л 	М м 	Н н 	О о 	П п 	Р р 	С с 	Т т 	У у 	Ф ф
      Х х 	Ц ц 	Ч ч 	Ш ш 	Щ щ 	Ъ ъ 	Ы ы 	Ь ь 	Э э 	Ю ю 	Я я
      """
    ]

  for_ strings $ \s =>
    let buffer : Buffer = fromString s
        converted : String = show buffer
    in
      when (s /= converted) $ putStrLn "Original: \{s}\nConverted: \{converted}"

