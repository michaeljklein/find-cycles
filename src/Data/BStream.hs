{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.BStream where

import Control.Comonad
import Control.Monad
import Control.Comonad.Cofree
import Data.Pair
import Data.Coerce
import Data.Bifunctor

-- | Infinite complete binary trees
newtype BStream a = BStream
  { runBStream :: Cofree Pair a
  } deriving (Functor)

instance Comonad BStream where
  extract = extract . runBStream
  duplicate = BStream . fmap BStream . duplicate . runBStream
  extend f = BStream . extend (f . BStream) . runBStream

deriving instance ComonadCofree Pair BStream

-- | @`paths` `bools`@ is the list of all infinite binary sequences.
-- Well, kind of.. it'a really just @`repeat` `False`@, but if we could
-- complete collecting infinite series in finite time, we could list them all.
bools :: Pair (BStream Bool)
bools =
  Pair . join bimap BStream $ (False :< coerce bools, True :< coerce bools)

-- | Scan along the results of a `BStream` with a `Semigroup` and
-- a method to convert to it.
scanMap :: Semigroup s => (a -> s) -> BStream a -> BStream s
scanMap f ~(BStream (x :< xs)) =
  let y = f x
   in BStream $ y :< fmap (runBStream . fmap (y <>) . scanMap f . BStream) xs

-- | Use a map and step to scan a `BStream`
stepMap :: (a -> b -> b) -> (a -> b) -> BStream a -> BStream b
stepMap stp f ~(BStream (x :< xs)) =
  let x' = f x
   in BStream $ x' :< fmap (loop x') xs
  where
    loop y ~(z :< zs) =
      let z' = stp z y
       in z' :< fmap (loop z') zs

-- | All levels of a `BStream` (infinite)
blevels :: BStream a -> [[a]]
blevels ~(BStream (x :< (Pair (xs, ys)))) =
  [x] : zipWith (++) (blevels $ BStream xs) (blevels $ BStream ys)

-- | The @n@-th level of a `BStream`
blevelN :: Int -> BStream a -> [a]
blevelN n
  | n < 0 = error $ "levelN: negative input: " ++ show n
blevelN 0 = return . extract
blevelN n = uncurry (++) . getPair . fmap (blevelN (n - 1)) . unwrap

