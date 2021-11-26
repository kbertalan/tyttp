module Main

import Test.Golden

basic : TestPool
basic = MkTestPool "Basic examples" [] Nothing [ "basic", "errors", "url" ]

node : TestPool
node = MkTestPool "Node code" [] (Just Node) [ "server", "node/uri" ]

ext : TestPool
ext = MkTestPool "Extensions" [] Nothing [ "ext/buffer" ]

main : IO ()
main = runner [ basic, node, ext ]
