{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Semigroup.Free where

import GHC.Generics
import GHC.Read
import Control.Applicative
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty(..))
import Data.Coerce
import Data.List (sortOn)
import Control.Monad
import Control.Monad.Free (Free(..), MonadFree(..), iterA)
import Control.Monad.Free.Church (F(..))
import qualified Control.Monad.Free.Church as F
import Data.Pair

import Criterion
import Criterion.Main
import Criterion.Types
import Data.Proxy (Proxy(..), asProxyTypeOf)
import Statistics.Types

import Weigh (Weigh, func, mainWith, wgroup)

-- | Size notes:
--
-- @
--  So a single element weighs ~400 bytes.
--    Supposing a MArray has a 1-byte overhead per element,
--    could we get a ~2 byte overhead per element if we stored every 400 elements in a MArray?
--    Ehh.. I don't know if the price of copying arrays will offset the cost..
--
--  What if we only access one cycle element at a time?
--    In that case, it's probably better to simply carry around a list and cons onto it.
--
--  What if we have a predictable cycle-size?
--    In that case, it's probably best to use pre-allocated memory
--    Possibly a pool of smaller pre-allocated chunks
-- @
--
sizeNotes :: ()
sizeNotes = ()

-- | Note:
--
-- @
--  On one hand, it seems nice to preserve the commutativity of the GappedIxCycle's
--  On the other hand, it takes O(min n m) time to zip two GappedIxCycle's together.
--  On the other hand, I'm not sure it needs to be anything other than a []/Seq/Set of period-1 cycles whose LCM is the given period. (that would give O(1) (<>))
-- @
--
notes :: ()
notes = ()

---- A strict finite collection of single-element cycles with:
---- O(1) `return`, `cons`, `snoc`, and `mappend`.
----
---- Pretty much everything else is much slower than lists.
---- (This data type's main features are its strictness and ability to `mappend`
---- two structures in constant time.)
----
---- Isomorphic to a strict version of:
----
---- @
----  GappedCycle a == Free Pair (Int, a)
----  wrap :: Pair (Free Pair (Int, a)) -> Free Pair (Int, a)
---- @
----
---- Which is effectively "pairing for free", exactly what we want.
----
---- Take a look at this:
----
---- @
----  F f a = F { runF :: forall r. (a -> r) -> (f r -> r) -> r }
----  F Pair a = F { runF :: forall r. (a -> r) -> (Pair r -> r) -> r }
----  F Pair a = F { runF :: forall r. (a -> r) -> (r -> r -> r) -> r }
----  F Pair a = F { runF :: forall r. (a -> r) -> (r -> r -> r) -> r }
----  F Pair a = F { runF :: forall r. Semigroup r => (a -> r) -> r }
----  F Pair a = FreeSemigroup a
---- @
----
----
--data GappedCycle a where
--  GCPure :: !Int -> !a -> GappedCycle a -- Period, Value
--  GCMappend :: !(GappedCycle a) -> !(GappedCycle a) -> GappedCycle a
--
--
-- |
--
-- @
--  GappedCycle a == Free Pair (Int, a)
-- @
--
-- data IntGappedCycle where
--   IGCPure :: !Int -> !Int -> IntGappedCycle -- Period, Value
--   IGCMappend :: !IntGappedCycle -> !IntGappedCycle -> IntGappedCycle
--


-- fun idea to test collection of bijections:
-- - make graph of bijections
-- - randomly traverse the graph until loop
-- - test equality with looped

-- If there are no edges, fails with an error.
-- Otherwise:
-- - randomly select a vertex with positive degree
-- - randomly select an adjacent vertex
-- - if the randomly selected vertex has been visited already, return all previous values
-- - otherwise add to collection of visited
-- randomCycleFrom :: Graph a -> m [a]

-- Interesting:
--   Store an array as a Set and a multi-permutation (index table) -- for fast updates, order, and no-repeated values
--   Or an ordered array and a (muti)-permutation -- read/write is fast, bsearch is fast, most combinations/alterations are very slow.


-- | `Alternative` without `empty`
class Applicative f => Semialternative f where
  (<!>) :: f a -> f a -> f a

  saconcat :: NonEmpty (f a) -> f a
  saconcat ~(x :| xs) =
    case xs of
      [] -> x
      ~(y:ys) -> x <!> saconcat (y :| ys)

  saconcatMap :: (a -> f b) -> NonEmpty a -> f b
  saconcatMap f = saconcat . fmap f

instance Semialternative NonEmpty where
  (<!>) = (<>)


-- | Wrap a `Semialternative` to provide a `Semigroup` instance
newtype WrapSemialternative f a = WrapSemialternative { runWrapSemialternative :: f a }

instance Semialternative f => Semigroup (WrapSemialternative f a) where
  {-# INLINE (<>) #-}
  (<>) :: WrapSemialternative f a -> WrapSemialternative f a -> WrapSemialternative f a
  (<>) x y = coerce $ coerce x <!> (coerce `asTypeOf` runWrapSemialternative) y


-- | A free `Semigroup` is a `Semialternative` that we can `foldMap1` over
-- (`foldMap` specialized to non-empty inputs) and convert to any other
-- `SemigroupFree` (they're all isomorphic to each other).
class Semialternative f => SemigroupFree f where
  foldMap1 :: Semigroup s => (a -> s) -> f a -> s
  foldMap1 f xs = runFSemigroup (semigroupFree xs) f

  semigroupFree :: SemigroupFree g => f a -> g a
  semigroupFree = runWrapSemialternative . foldMap1 (WrapSemialternative . pure)

  {-# MINIMAL foldMap1 | semigroupFree #-}

instance SemigroupFree NonEmpty where
  foldMap1 f ~(x :| xs) = loop x xs
    where
      loop x [] = f x
      loop x ~(y:ys) = f x <> loop x ys

-- | From the triangle benchmakrs, this is the fastest.
-- How fast? @~100x@ faster than `NonEmpty`, taking
-- @~180 ms@ to construct and destruct a triangle with
-- around a million elements.
--
-- For a little comparison, the average human blink of an eye
-- is around @100-400 ms@ (according to WolframAlpha). I can
-- process a trangular collection of a million elements in less
-- then half the average time that it takes to blink an eye!
--
-- That's @~180 ns@ per element..
--
-- My computer has a @3.1 GHz@ processor. Assuming total utilization
-- of a single core, we get a cycle-duration of @~0.323 ns@. That
-- gives us an estimate of @557@ cycles per element.
--
-- That's not the greatest. I'll have to check CPU utilization for that benchmark to see if it's skewing my quick math.
-- (267.2%?, not sure what to make of that. need to run without criterion to remove overhead)
-- Though it's still not bad for a pure, polymorphic data type.
--
-- All in all, this data type is plenty fast for its primary purpose:
-- quickly accumulating cycle-pieces when calculating the final cycle.
data FreeSemigroup a where
  PureFreeSemigroup :: a -> FreeSemigroup a
  AppendFreeSemigroup :: FreeSemigroup a -> FreeSemigroup a -> FreeSemigroup a
  deriving (Eq, Ord, Read, Show, Functor, Generic, Generic1)

instance Semigroup (FreeSemigroup a) where
  {-# INLINE (<>) #-}
  (<>) = AppendFreeSemigroup

instance Applicative FreeSemigroup where
  {-# INLINE pure #-}
  pure = PureFreeSemigroup

  PureFreeSemigroup f <*> xs = f <$> xs
  f <*> PureFreeSemigroup x = ($ x) <$> f
  ~(AppendFreeSemigroup fs gs) <*> ~(AppendFreeSemigroup xs ys) = AppendFreeSemigroup (fs <*> xs) (gs <*> ys)

instance Semialternative FreeSemigroup where
  {-# INLINE (<!>) #-}
  (<!>) = AppendFreeSemigroup

instance SemigroupFree FreeSemigroup where
  semigroupFree (PureFreeSemigroup x) = pure x
  semigroupFree (AppendFreeSemigroup xs ys) = semigroupFree xs <!> semigroupFree ys

-- | `semigroupFree` specialized to `FreeSemigroup`
{-# INLINE toFreeSemigroup #-}
toFreeSemigroup :: SemigroupFree f => f a -> FreeSemigroup a
toFreeSemigroup = semigroupFree

-- | It looks like this sucks around the [2^12..2^16] elements range
data FreeSemigroup' a where
  PureFreeSemigroup' :: !a -> FreeSemigroup' a
  AppendFreeSemigroup' :: !(FreeSemigroup' a) -> !(FreeSemigroup' a) -> FreeSemigroup' a
  deriving (Eq, Ord, Read, Show, Functor, Generic, Generic1)

instance Semigroup (FreeSemigroup' a) where
  {-# INLINE (<>) #-}
  (<>) = AppendFreeSemigroup'

instance Applicative FreeSemigroup' where
  {-# INLINE pure #-}
  pure !x = PureFreeSemigroup' x

  PureFreeSemigroup' f <*> xs = f <$> xs
  f <*> PureFreeSemigroup' x = ($ x) <$> f
  ~(AppendFreeSemigroup' fs gs) <*> ~(AppendFreeSemigroup' xs ys) = AppendFreeSemigroup' (fs <*> xs) (gs <*> ys)

instance Semialternative FreeSemigroup' where
  {-# INLINE (<!>) #-}
  (<!>) = AppendFreeSemigroup'

instance SemigroupFree FreeSemigroup' where
  semigroupFree (PureFreeSemigroup' x) = pure x
  semigroupFree (AppendFreeSemigroup' xs ys) = semigroupFree xs <!> semigroupFree ys


-- | `Free`-based `FreeSemigroup`
newtype FreePS a = FreePS { runFreePS :: Free Pair a } deriving (Eq, Ord, Show, Read, Functor, Applicative, Monad)

deriving instance MonadFree Pair FreePS

instance Semialternative FreePS where
  (<!>) = fmap (wrap . Pair) . (,)

instance SemigroupFree FreePS where
  semigroupFree = iterA (uncurry (<!>) . getPair) . runFreePS


-- | `F`-based `FreeSemigroup`
newtype FPS a = FPS { runFPS :: F Pair a } deriving (Functor, Applicative, Monad)

deriving instance MonadFree Pair FPS

instance Semialternative FPS where
  (<!>) = fmap (wrap . Pair) . (,)

instance SemigroupFree FPS where
  semigroupFree = F.iter (uncurry (<!>) . getPair) . fmap pure . runFPS


-- | Partially applied `foldMap1` wrapped in a @newtype@.
newtype FSemigroup a = FSemigroup { runFSemigroup :: forall s. Semigroup s => (a -> s) -> s } deriving (Functor)

instance Eq a => Eq (FSemigroup a) where
  {-# INLINE (==) #-}
  x == y = toFreeSemigroup x == toFreeSemigroup y

instance Ord a => Ord (FSemigroup a) where
  {-# INLINE compare #-}
  compare x y = toFreeSemigroup x `compare` toFreeSemigroup y

instance Show a => Show (FSemigroup a) where
  {-# INLINE show #-}
  show = show . toFreeSemigroup

instance Semigroup (FSemigroup a) where
  {-# INLINE (<>) #-}
  FSemigroup x <> FSemigroup y = FSemigroup $ \cnv -> y cnv <> y cnv

instance Applicative FSemigroup where
  {-# INLINE pure #-}
  pure x = FSemigroup $ \cnv -> cnv x

  {-# INLINE liftA2 #-}
  liftA2 f (FSemigroup xs) (FSemigroup ys) = FSemigroup $ \cnv -> ys . xs $ fmap cnv . f

instance Semialternative FSemigroup where
  {-# INLINE (<!>) #-}
  FSemigroup x <!> FSemigroup y = FSemigroup $ \cnv -> x cnv <> y cnv

instance SemigroupFree FSemigroup where
  {-# INLINE foldMap1 #-}
  foldMap1 cnv (FSemigroup xs) = xs cnv

-- | `FSemigroup` with explicitly provided @(`<>`)@.
--
-- Slightly faster in triangle benchmarks.
newtype FS a = FS
  { runFS :: forall s. (s -> s -> s) -> (a -> s) -> s
  } deriving (Functor)

instance Eq a => Eq (FS a) where
  {-# INLINE (==) #-}
  x == y = toFreeSemigroup x == toFreeSemigroup y

instance Ord a => Ord (FS a) where
  {-# INLINE compare #-}
  compare x y = toFreeSemigroup x `compare` toFreeSemigroup y

instance Show a => Show (FS a) where
  {-# INLINE show #-}
  show = show . toFreeSemigroup

instance Semigroup (FS a) where
  {-# INLINE (<>) #-}
  FS x <> FS y = FS $ \append cnv -> x append cnv `append` y append cnv

instance Applicative FS where
  {-# INLINE pure #-}
  pure x = FS $ \append cnv -> cnv x

  {-# INLINE liftA2 #-}
  liftA2 f (FS xs) (FS ys) =
    FS $ \append cnv -> ys append . xs (liftA2 append) $ fmap cnv . f

instance Semialternative FS where
  {-# INLINE (<!>) #-}
  FS x <!> FS y = FS $ \append cnv -> x append cnv `append` y append cnv

instance SemigroupFree FS where
  {-# INLINE foldMap1 #-}
  foldMap1 cnv (FS xs) = xs (<>) cnv


-- | Lift function application to strict function application
{-# INLINE liftS2 #-}
liftS2 :: (a -> b -> c) -> a -> b -> c
liftS2 f x y = f x' y'
  where
    !x' = x
    !y' = y

-- | Strict `FSemigroup` with explicitly provided @(`<>`)@.
newtype FS' a = FS'
  { runFS' :: forall s. (s -> s -> s) -> (a -> s) -> s
  } deriving (Functor)

instance Eq a => Eq (FS' a) where
  {-# INLINE (==) #-}
  x == y = toFreeSemigroup x == toFreeSemigroup y

instance Ord a => Ord (FS' a) where
  {-# INLINE compare #-}
  compare x y = toFreeSemigroup x `compare` toFreeSemigroup y

instance Show a => Show (FS' a) where
  {-# INLINE show #-}
  show = show . toFreeSemigroup

instance Semigroup (FS' a) where
  {-# INLINE (<>) #-}
  FS' x <> FS' y = FS' $ \append cnv -> x append cnv `append` y append cnv

instance Applicative FS' where
  {-# INLINE pure #-}
  pure x = let !x' = x in FS' $ \_ cnv -> cnv x'

  {-# INLINE liftA2 #-}
  liftA2 f (FS' xs) (FS' ys) =
    FS' $ \append cnv -> ys append . xs (liftA2 append) $ fmap cnv . f

instance Semialternative FS' where
  {-# INLINE (<!>) #-}
  FS' x <!> FS' y = FS' $ \append cnv -> x append cnv `append` y append cnv

instance SemigroupFree FS' where
  {-# INLINE foldMap1 #-}
  foldMap1 cnv (FS' xs) = xs (liftS2 (<>)) (cnv $!)


-- | A concatenated "triangle" of values:
--
-- @
--  saconcat [1] <> saconcat [1, 2] <> saconcat [1, 2, 3] <> .. <> saconcat [1..n]
-- @
--
makeTriangle :: Semialternative f => Int -> f Int
makeTriangle n | n < 1 = error "makeTriangle: non-positive input"
makeTriangle 1 = pure 1
makeTriangle n = makeTriangle (n - 1) <!> makeLine n

-- | Conveniently construct and `Sum` a triangle of `SemigroupFree` values
-- for benchmarking.
destructTriangle :: SemigroupFree f => Proxy f -> Int -> Int
destructTriangle prxy = getSum . foldMap1 Sum . (`asProxyTypeOf` proxyApInt prxy) . makeTriangle

-- | A concatenated line of values
--
-- @
--  saconcat [1..n]
-- @
makeLine :: Semialternative f => Int -> f Int
makeLine n = saconcatMap pure [1..n]

-- | Conveniently construct and `Sum` a line of `SemigroupFree` values
-- for benchmarking.
destructLine :: SemigroupFree f => Proxy f -> Int -> Int
destructLine prxy = getSum . foldMap1 Sum . (`asProxyTypeOf` proxyApInt prxy) . makeLine

-- | Apply `Int` to a type inside a @proxy@
proxyApInt :: proxy g -> Proxy (g Int)
proxyApInt _ = Proxy

-- | Benchmark `destructTriangle`
benchTriangle :: SemigroupFree f => Proxy f -> Int -> Benchmarkable
benchTriangle prxy !x = nf (destructTriangle prxy) x

-- | Benchmark `destructLine`
benchLine :: SemigroupFree f => Proxy f -> Int -> Benchmarkable
benchLine prxy !x = nf (destructLine prxy) x


-- | Weigh `destructTriangle`
weighTriangle :: SemigroupFree f => String -> Proxy f -> Int -> Weigh ()
weighTriangle name prxy !sz = func desc' (destructTriangle prxy) sz
  where
    desc' = unwords [name, "destructTriangle, size:", show sz]

-- | Weigh `destructLine`
weighLine :: SemigroupFree f => String -> Proxy f -> Int -> Weigh ()
weighLine name prxy !sz = func desc' (destructLine prxy) sz
  where
    desc' = unwords [name, "destructLine    , size:", show sz]


-- | `weighTriangle` and `weighLine` for powers of @2@
-- in experimentally derived ranges.
weighPow2s :: SemigroupFree f => String -> Proxy f -> Weigh ()
weighPow2s name prxy = wgroup name $ do
  forM_ ([5..15] :: [Int]) $ weighLine name prxy . (2 ^)
  forM_ ([1..10] :: [Int]) $ weighTriangle name prxy . (2 ^)


type Est = Estimate ConfInt Double

reportEstimate :: Report -> Est
reportEstimate = anMean . reportAnalysis

estimate :: Benchmarkable -> IO Est
estimate = fmap reportEstimate . benchmark'

quickEstimate :: Benchmarkable -> IO Est
quickEstimate = fmap reportEstimate . benchmarkWith' quickConfig

quickConfig :: Config
quickConfig =
  defaultConfig {confInterval = cl90, timeLimit = 1, resamples = 100}



-- | `benchTriangle` and `benchLine` for powers of @2@
-- in experimentally derived ranges.
benchPow2s :: SemigroupFree f => String -> Proxy f -> Benchmark
benchPow2s name prxy =
  bgroup name $
  ((\n -> (name ++ " Triangle: " ++ show n) `bench` benchTriangle prxy n) .
   (2 ^) <$>
   [1 .. 10]) ++
  ((\n -> (name ++ " Line: " ++ show n) `bench` benchLine prxy n) . (2 ^) <$>
   [5 .. 15])


-- | `sequenceA` a benchmark over all of the `SemigroupFree` implementations
benchable :: Applicative f => (forall g. SemigroupFree g => String -> Proxy g -> f a) -> f [a]
benchable f = sequenceA
  [ f "NonEmpty      " (Proxy @NonEmpty)
  , f "FreeSemigroup " (Proxy @FreeSemigroup)
  , f "FreeSemigroup'" (Proxy @FreeSemigroup')
  , f "FreePS        " (Proxy @FreePS)
  , f "FPS           " (Proxy @FPS)
  , f "FSemigroup    " (Proxy @FSemigroup)
  , f "FS            " (Proxy @FS)
  , f "FS'           " (Proxy @FS')
  ]


weighFS :: Weigh ()
weighFS = void $ benchable weighPow2s

runWeighFS :: IO ()
-- runWeighFS = mainWith $ weighPow2s "FS" (Proxy @FS)
runWeighFS = mainWith weighFS

benchCompare :: Int -> (forall g. SemigroupFree g => Proxy g -> Benchmarkable) -> IO ()
benchCompare n f = do
  results <- sortOn (estPoint . snd) <$> benchable (\n prxy -> (n, ) <$> estimate (f prxy))
  let resultNames = fst <$> results
  appendFile "orderings.txt" . unlines $ show n : resultNames ++ [""]
  mapM_ (\(n, e) -> putStrLn n >> putStrLn ("  " ++ show e)) results


benchFS :: [Benchmark]
benchFS = runIdentity $ benchable (fmap Identity . benchPow2s)


runBenchFS :: IO ()
runBenchFS =
  forM_ ([8,8+40..1414] :: [Int]) $ \i -> do
    putStrLn ""
    putStrLn $
      unwords
        [ "Benchmarking Triangle with input:"
        , show i
        , "(size"
        , show (sum [n | n <- [1 .. i]])
        , ")"
        ]
    benchCompare i (`benchTriangle` i)

