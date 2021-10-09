module TyTTP.Handler

import TyTTP.Request
import TyTTP.Response

public export
record Step ma ha (f : ma -> Type -> Type) hb a b where
  constructor MkStep
  request : Request ma ha f a
  response : Response hb b

export
Functor (Step ma ha f hb a) where
  map f step = record { response $= map f } step

export
Bifunctor (Step ma ha TyTTP.Request.simpleBody hb) where
  bimap f g step = record { request $= map f, response $= map g } step

public export
Handler : (monad : Type -> Type)
  -> (methodReqFrom: Type)
  -> (headerReqFrom: Type)
  -> (bodyReqFnFrom: methodReqFrom -> Type -> Type)
  -> (headerResFrom: Type)
  -> (bodyReqFrom: Type)
  -> (bodyResFrom: Type)
  -> (methodReqTo: Type)
  -> (headerReqTo: Type)
  -> (bodyReqFnTo: methodReqTo -> Type -> Type)
  -> (headerResTo: Type)
  -> (bodyReqTo: Type)
  -> (bodyResTo: Type)
  -> Type
Handler monad
  methodReqFrom headerReqFrom bodyReqFnFrom headerResFrom bodyReqFrom bodyResFrom
  methodReqTo headerReqTo bodyReqFnTo headerResTo bodyReqTo bodyResTo
  = Step
    methodReqFrom headerReqFrom bodyReqFnFrom headerResFrom bodyReqFrom bodyResFrom
  -> monad $ Step
    methodReqTo headerReqTo bodyReqFnTo headerResTo bodyReqTo bodyResTo
