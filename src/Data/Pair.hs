{-# LANGUAGE DeriveFunctor #-}

module Data.Pair where

import Control.Monad (join)
import Data.Functor.Classes
import GHC.Read

-- | A lazy pair of values
newtype Pair a = Pair { getPair :: (a, a) } deriving (Functor)

instance Eq1 Pair where
  liftEq eq ~(Pair (x, y)) ~(Pair (z, w)) = eq x z && eq y w

instance Eq a => Eq (Pair a) where
  (==) = eq1

instance Ord1 Pair where
  liftCompare cmp ~(Pair (x, y)) ~(Pair (z, w)) = cmp x z <> cmp y w

instance Ord a => Ord (Pair a) where
  compare = compare1

instance Show1 Pair where
  liftShowsPrec sp _ n ~(Pair (x, y)) = showsBinaryWith sp sp "Pair" n x y

instance Show a => Show (Pair a) where
  showsPrec = showsPrec1

instance Read1 Pair where
  liftReadPrec rp _ = readBinaryWith rp rp "Pair" (fmap Pair . (,))

instance Read a => Read (Pair a) where
  readPrec = readPrec1

instance Applicative Pair where
  pure = Pair . join (,)

  ~(Pair (f, g)) <*> ~(Pair (x, y)) = Pair (f x, g y)


