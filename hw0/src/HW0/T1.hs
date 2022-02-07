{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module HW0.T1 where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left x) = (Left x, Left x)
distrib (Right (y, z)) = (Right y, Right z)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(x, (y, z)) -> ((x, y), z)) (\((x, y), z) -> (x, (y, z)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither =
  Iso
    ( \case
        Left x -> Left (Left x)
        Right (Left y) -> Left (Right y)
        Right (Right z) -> Right z
    )
    ( \case
        Left (Left x) -> Left x
        Left (Right y) -> Right (Left y)
        Right z -> Right (Right z)
    )
