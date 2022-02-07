module HW2.T5
  ( ExceptState (..),
    EvaluationError (..),
    mapExceptState,
    wrapExceptState,
    joinExceptState,
    modifyExceptState,
    throwExceptState,
    eval,
  )
where

import Control.Monad (ap)
import HW2.T1
import HW2.T4 hiding (eval)

data ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f s =
  let sf = runES s
   in ES $ \x ->
        let r = sf x
         in case r of
              Success (a :# e) -> Success (f a :# e)
              Error e -> Error e

wrapExceptState :: a -> ExceptState e s a
wrapExceptState x = ES $ \s -> Success (x :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState s =
  let sf = runES s
   in ES $ \x ->
        let r = sf x
         in case r of
              (Success ((ES g) :# t)) -> g t
              (Error e) -> Error e

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval e = case e of
  Val d -> pure d
  Op (Add x y) -> do
    x_ <- eval x
    y_ <- eval y
    modifyExceptState (Add x_ y_ :)
    return $ x_ + y_
  Op (Sub x y) -> do
    x_ <- eval x
    y_ <- eval y
    modifyExceptState (Sub x_ y_ :)
    return $ x_ - y_
  Op (Mul x y) -> do
    x_ <- eval x
    y_ <- eval y
    modifyExceptState (Mul x_ y_ :)
    return $ x_ * y_
  Op (Div x y) -> do
    x_ <- eval x
    y_ <- eval y
    case y_ of
      0 -> throwExceptState DivideByZero
      _ -> do
        modifyExceptState (Div x_ y_ :)
        return $ x_ / y_
  Op (Abs x) -> do
    x_ <- eval x
    modifyExceptState (Abs x_ :)
    return $ abs x_
  Op (Sgn x) -> do
    x_ <- eval x
    modifyExceptState (Sgn x_ :)
    return $ signum x_
