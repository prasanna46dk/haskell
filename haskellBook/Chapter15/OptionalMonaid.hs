module OptionalMonoid where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

fromOnly Nada = error "Nothing"
fromOnly (Only a) = a

-- Only works for int need to find way to work for any type
instance (Monoid a, Num a)
    => Monoid (Optional a) where
    mempty = Nada
    mappend x Nada = x
    mappend Nada x = x
    mappend x y = Only(fromOnly x + fromOnly y)
