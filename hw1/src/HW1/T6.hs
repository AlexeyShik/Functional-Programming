module HW1.T6
  ( epart
  , mcat
  ) where

-- Не Foldable методы использовать нельзя, поэтому я выразил их через Foldable--

_mconcat :: Monoid a => [a] -> a
_mconcat = foldr mappend mempty

_catMaybes :: [Maybe a] -> [a]
_catMaybes = foldr combiner mempty
  where
    combiner Nothing accum = accum
    combiner (Just x) accum = x : accum

mcat :: Monoid a => [Maybe a] -> a
mcat = _mconcat . _catMaybes

_lefts :: [Either a b] -> [a]
_lefts = foldr combiner mempty
  where
    combiner (Right _) accum = accum
    combiner (Left x) accum = x : accum

_rights :: [Either a b] -> [b]
_rights = foldr combiner mempty
  where
    combiner (Left _) accum = accum
    combiner (Right x) accum = x : accum

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart eithers = ((_mconcat . _lefts) eithers, (_mconcat . _rights) eithers)
