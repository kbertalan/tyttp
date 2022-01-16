module Main

import Test.Golden

url : TestPool
url = MkTestPool "URL tests" [] Nothing [ "url" ]

node : TestPool
node = MkTestPool "Node code" [] (Just Node) [ "server", "node/uri" ]

ext : TestPool
ext = MkTestPool "Extensions" [] Nothing [ "ext/buffer" ]

main : IO ()
main = runner [ url, node, ext ]
