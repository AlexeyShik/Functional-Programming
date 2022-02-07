module HW2.T4
  ( Prim (..),
    Expr (..),
    State (..),
    mapState,
    wrapState,
    joinState,
    modifyState,
    eval,
  )
where

import Control.Monad (ap)
import HW2.T1

data State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState f s =
  let sf = runS s
   in S $ \x ->
        let (a :# e) = sf x
         in f a :# e

wrapState :: a -> State s a
wrapState x = S $ \s -> x :# s

joinState :: State s (State s a) -> State s a
joinState s =
  let sf = runS s
   in S $ \x ->
        let ((S g) :# t) = sf x
         in g t

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a
  = Add a a -- (+)
  | Sub a a -- (-)
  | Mul a a -- (*)
  | Div a a -- (/)
  | Abs a -- abs
  | Sgn a -- signum

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval e = case e of
  Val d -> pure d
  Op (Add x y) -> do
    x_ <- eval x
    y_ <- eval y
    modifyState (Add x_ y_ :)
    return $ x_ + y_
  Op (Sub x y) -> do
    x_ <- eval x
    y_ <- eval y
    modifyState (Sub x_ y_ :)
    return $ x_ - y_
  Op (Mul x y) -> do
    x_ <- eval x
    y_ <- eval y
    modifyState (Mul x_ y_ :)
    return $ x_ * y_
  Op (Div x y) -> do
    x_ <- eval x
    y_ <- eval y
    modifyState (Div x_ y_ :)
    return $ x_ / y_
  Op (Abs x) -> do
    x_ <- eval x
    modifyState (Abs x_ :)
    return $ abs x_
  Op (Sgn x) -> do
    x_ <- eval x
    modifyState (Sgn x_ :)
    return $ signum x_
