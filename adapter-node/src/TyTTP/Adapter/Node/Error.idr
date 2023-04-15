module TyTTP.Adapter.Node.Error

import Node.Error as Node
import TyTTP

export
Node.ErrorClass e => Error e where
  message = Node.(.message)

