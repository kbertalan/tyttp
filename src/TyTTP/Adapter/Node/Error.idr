module TyTTP.Adapter.Node.Error

import Node.Error as Node
import TyTTP

export
Error Node.Error where
  message = Node.Error.message

