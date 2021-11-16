module TyTTP.Core.Error


public export
interface Error e where
  message : e -> String

Error String where
  message = id

