module HW1.T5
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr combiner ([] :| [])
  where
    combiner c (x :| xs)
      | c == sep = [] :| (x : xs)
      | otherwise = (c : x) :| xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (x :| []) = x
joinWith sep (x :| xs) = helper (x : xs)
  where
    helper [] = []  -- Impossible state
    helper [c] = c
    helper (c : cs) = c ++ [sep] ++ helper cs
