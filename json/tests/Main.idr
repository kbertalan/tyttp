module Main

import Test.Golden

json : TestPool
json = MkTestPool "json" [] (Just Node) [ "json" ]

main : IO ()
main = runner [ json ]

