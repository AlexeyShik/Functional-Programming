module HW1.T2
  ( N (..)
  , ncmp
  , ndiv
  , nEven
  , nFromNatural
  , nmod
  , nmult
  , nOdd
  , nplus
  , nsub
  , nToNum
  ) where

import GHC.Natural (Natural)

data N 
  = Z 
  | S N deriving (Show, Eq)

nplus :: N -> N -> N
nplus x Z = x
nplus x (S y) = S $ nplus x y

nmult :: N -> N -> N
nmult _ Z = Z
nmult x (S y) = nplus x (nmult x y)

nsub :: N -> N -> Maybe N
nsub x Z = Just x
nsub Z _ = Nothing
nsub (S x) (S y) = nsub x y

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z _ = LT
ncmp _ Z = GT
ncmp (S x) (S y) = ncmp x y

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S $ nFromNatural (x - 1)

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S x) = (+ 1) $ nToNum x

nEven, nOdd :: N -> Bool
nEven Z = True
nEven (S x) = not $ nEven x
nOdd x = not $ nEven x

fromJust :: Maybe N -> N
fromJust Nothing = error "Expected Just, found Nothing"
fromJust (Just x) = x

ndiv :: N -> N -> N
ndiv _ Z = error "Divison by zero"
ndiv x y
  | (==) (ncmp x y) LT = Z
  | otherwise = nplus (S Z) (ndiv (fromJust (nsub x y)) y)

nmod :: N -> N -> N
nmod _ Z = error "Divison by zero"
nmod x y = fromJust $ nsub x (nmult (ndiv x y) y)
