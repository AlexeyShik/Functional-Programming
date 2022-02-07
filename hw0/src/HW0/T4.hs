module HW0.T4 where

import Data.Function (fix)
import GHC.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b]
map' = fix $
  \f g lst ->
    case lst of
      [] -> []
      x : xs -> g x : f g xs

fibHelper :: (Natural, Natural, Natural) -> Natural
fibHelper = fix $
  \f (lst, cur, n) ->
    case n of
      0 -> lst
      _ -> f (cur, lst + cur, n - 1)

fib :: Natural -> Natural
fib n = fibHelper (0, 1, n)

fac :: Natural -> Natural
fac = fix $
  \f n ->
    case n of
      0 -> 1
      _ -> n * f (n - 1)
