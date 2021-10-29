module Data.Either.Ext

public export
data IsRight : Either a b -> Type where
  ItIsRight : IsRight (Right x)

export
Uninhabited (IsRight (Left x)) where
  uninhabited ItIsRight impossible

public export
data IsLeft : Either a b -> Type where
  ItIsLeft : IsLeft (Left x)

export
Uninhabited (IsLeft (Right x)) where
  uninhabited ItIsLeft impossible

