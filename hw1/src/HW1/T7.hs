module HW1.T7
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a 
  = a :+ ListPlus a 
  | Last a deriving (Show, Eq)

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last x0) y = x0 :+ y
  (<>) (x :+ xs) y = x :+ (<>) xs y

data Inclusive a b 
  = This a 
  | That b 
  | Both a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This x) (This _x) = This $ x <> _x
  (<>) (This x) (That y) = Both x y
  (<>) (This x) (Both _x y) = Both (x <> _x) y
  (<>) (That y) (This x) = Both x y
  (<>) (That y) (That _y) = That $ y <> _y
  (<>) (That y) (Both x _y) = Both x (y <> _y)
  (<>) (Both x y) (This _x) = Both (x <> _x) y
  (<>) (Both x y) (That _y) = Both x (y <> _y)
  (<>) (Both x y) (Both _x _y) = Both (x <> _x) (y <> _y)

newtype DotString = DS String deriving (Show, Eq)

instance Semigroup DotString where
  (<>) (DS "") y = y
  (<>) x (DS "") = x
  (<>) (DS a) (DS b) = DS (a ++ "." ++ b)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F f) (F g) = F $ f . g

instance Monoid (Fun a) where
  mempty = F id
