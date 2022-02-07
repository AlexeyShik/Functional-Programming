module HW0.T2 where

import Data.Void (Void)

type Not a = a -> Void

doubleNeg :: a -> Not (Not a) -- a -> (a -> Void) -> Void
doubleNeg x f = f x

reduceTripleNeg :: Not (Not (Not a)) -> Not a -- (Not (Not a) -> Void) -> a -> Void
reduceTripleNeg f x =
  let g = doubleNeg x
   in f g
