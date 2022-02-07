module HW2.T2
  ( distOption,
    distPair,
    distQuad,
    distAnnotated,
    distExcept,
    distPrioritised,
    distStream,
    distList,
    distFun,
    wrapOption,
    wrapPair,
    wrapQuad,
    wrapAnnotated,
    wrapExcept,
    wrapPrioritised,
    wrapStream,
    wrapList,
    wrapFun,
    concatLists,
  )
where

import HW2.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _) = None
distOption (_, None) = None
distOption (Some a, Some b) = Some (a, b)

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x1 x2, P y1 y2) = P (x1, y1) (x2, y2)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q x1 x2 x3 x4, Q y1 y2 y3 y4) = Q (x1, y1) (x2, y2) (x3, y3) (x4, y4)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (x :# e1, y :# e2) = (x, y) :# e1 <> e2

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _) = Error e
distExcept (_, Error e) = Error e
distExcept (Success x, Success y) = Success (x, y)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low x, Low y) = Low (x, y)
distPrioritised (Low x, Medium y) = Medium (x, y)
distPrioritised (Low x, High y) = High (x, y)
distPrioritised (Medium x, Low y) = Medium (x, y)
distPrioritised (Medium x, Medium y) = Medium (x, y)
distPrioritised (Medium x, High y) = High (x, y)
distPrioritised (High x, Low y) = High (x, y)
distPrioritised (High x, Medium y) = High (x, y)
distPrioritised (High x, High y) = High (x, y)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x :> sx, y :> sy) = (x, y) :> distStream (sx, sy)

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (hx :. tx, y) = concatLists (toPairsList hx y) (distList (tx, y))

-- 1 [3, 4, 5] -> [(1, 3), (1, 4), (1, 5)]
toPairsList :: a -> List b -> List (a, b)
toPairsList _ Nil = Nil
toPairsList x (y :. ys) = (x, y) :. toPairsList x ys

concatLists :: List a -> List a -> List a
concatLists Nil y = y
concatLists x Nil = x
concatLists (x :. xs) y = x :. concatLists xs y

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F $ \i -> (f i, g i)

wrapOption :: a -> Option a
wrapOption = Some

wrapPair :: a -> Pair a
wrapPair x = P x x

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty

wrapExcept :: a -> Except e a
wrapExcept = Success

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x

wrapList :: a -> List a
wrapList x = x :. Nil

wrapFun :: a -> Fun i a
wrapFun x = F $ \_ -> x
