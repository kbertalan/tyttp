module TyTTP.Adapter.Node.Error

import Node.Error
import TyTTP

export
Error NodeError where
  message = Node.Error.message

