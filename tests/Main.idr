module Main

import Test.Golden

basic : TestPool
basic = MkTestPool "Basic examples" [] Nothing [ "basic", "errors" ]

node : TestPool
node = MkTestPool "Node code" [] (Just Node) [ "readable" ]

main : IO ()
main = runner [ basic, node ]
