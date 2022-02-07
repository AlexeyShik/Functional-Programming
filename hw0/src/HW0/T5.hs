module HW0.T5 where

import GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ = id

ns :: Nat a -> Nat a
ns n f n0 = f $ n f n0

nplus :: Nat a -> Nat a -> Nat a
nplus n m f n0 = n f (m f n0)

nmult :: Nat a -> Nat a -> Nat a
nmult n m f = n (m f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns $ nFromNatural (n - 1)

nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0
