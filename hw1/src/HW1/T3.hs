module HW1.T3
  ( Tree (..)
  , tdepth
  , tFromList
  , tinsert
  , tmember
  , tsize
  ) where

data Tree a 
  = Leaf 
  | Branch (Int, Int) (Tree a) a (Tree a) deriving (Show, Eq)

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch Leaf element Leaf = Branch (1, 1) Leaf element Leaf
mkBranch (Branch (s, h) l el r) element Leaf = Branch (s + 1, h + 1) (Branch (s, h) l el r) element Leaf
mkBranch Leaf element (Branch (s, h) l el r) = Branch (s + 1, h + 1) Leaf element (Branch (s, h) l el r)
mkBranch (Branch (s1, h1) l1 el1 r1) element (Branch (s2, h2) l2 el2 r2) =
  Branch (s1 + s2 + 1, (+ 1) $ max h1 h2) (Branch (s1, h1) l1 el1 r1) element (Branch (s2, h2) l2 el2 r2)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (size, _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (_, h) _ _ _) = h

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ left element right)
  | (==) a element = True
  | (<) a element = tmember a left
  | otherwise = tmember a right

leftRotate :: Tree a -> Tree a
leftRotate (Branch _ (Branch _ ll el lr) et r) = mkBranch ll el (mkBranch lr et r)
leftRotate x = x

rightRotate :: Tree a -> Tree a
rightRotate (Branch _ l et (Branch _ rl er rr)) = mkBranch (mkBranch l et rl) er rr
rightRotate x = x

fixL :: Tree a -> Tree a
fixL (Branch st (Branch sl ll el lr) et r)
  | tdepth ll >= tdepth lr = leftRotate (Branch st (Branch sl ll el lr) et r)
  | otherwise = leftRotate $ mkBranch (rightRotate (Branch sl ll el lr)) et r
fixL x = x

fixR :: Tree a -> Tree a
fixR (Branch st l et (Branch sr rl er rr))
  | tdepth rr >= tdepth rl = rightRotate (Branch st l et (Branch sr rl er rr))
  | otherwise = rightRotate $ mkBranch l et (leftRotate (Branch sr rl er rr))
fixR x = x

rebalance :: Tree a -> Tree a
rebalance Leaf = Leaf
rebalance (Branch (sz, h) Leaf x Leaf) = Branch (sz, h) Leaf x Leaf
rebalance (Branch sz l el r)
  | tdepth l > 1 + tdepth r = fixL $ Branch sz l el r
  | tdepth l + 1 < tdepth r = fixR $ Branch sz l el r
  | otherwise = Branch sz l el r

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a (Branch (size, h) left element right) =
  case compare a element of
    EQ -> Branch (size, h) left element right
    LT -> rebalance $ mkBranch (tinsert a left) element right
    GT -> rebalance $ mkBranch left element (tinsert a right)

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf
