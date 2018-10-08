{-# LANGUAGE DeriveFunctor #-}

module Data.List.Utils where

import Data.Bifunctor
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import GHC.Clock
import Data.Semigroup hiding (diff)
import Math.Combinatorics.Exact.Binomial
import Control.Monad
import Data.Foldable (foldl')
import GHC.Exts

-- Successive differences between elements of a list
diff :: Num a => [a] -> [a]
diff = zipWith(-) =<< drop 1 -- tail [] == undefined



-- | For a monotonically increasing function/series,
-- only output new records.
--
-- E.g.
--
-- @
--  monoRecordsOn id [1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3] = [1,2,3]
-- @
--
monoRecordsOn :: Eq b => (a -> b) -> [a] -> [a]
monoRecordsOn _ [] = []
monoRecordsOn f ~(x:xs) = x : loop (f x) xs
  where
    loop _ [] = []
    loop y ~(z:zs) =
      if y == z'
        then loop y zs
        else z : loop z' zs
      where
        z' = f z


-- | Is the list `eventually1s` or `eventually01s`, and `True` if `eventually1s`
eventually101 :: [Int] -> (Bool, [Int], Int)
eventually101 xs = case compare l01 l1 of
                     LT -> (True , xs1 , l1 )
                     _  -> (False, xs01, l01)
  where
    ~(xs01, l01) = eventually01s xs
    ~(xs1 , l1 ) = eventually1s  xs

-- | The number of elements of the list ending in @0, 1, 0, 1..@ and the rest of the list
eventually01s :: [Int] -> ([Int], Int)
eventually01s [] = ([], 0)
eventually01s ~(x:xs) = case x of
                          0 -> loop0 [0] 1 xs
                          1 -> loop1 [1] 1 xs
                          _ -> (x :) `first` eventually01s xs
  where
    loop0 :: [Int] -> Int -> [Int] -> ([Int], Int)
    loop0 _  i [] = ([], i)
    loop0 ps i ~(x:xs) = case x of
                        0 -> ((ps ++) . (x :)) `first` loop0 [0] 1 xs
                        1 -> loop1 (ps ++ [1]) (i + 1) xs
                        _ -> ((ps ++) . (x :)) `first` eventually01s xs

    loop1 :: [Int] -> Int -> [Int] -> ([Int], Int)
    loop1 _  i [] = ([], i)
    loop1 ps i ~(x:xs) = case x of
                        0 -> loop0 (ps ++ [0]) (i + 1) xs
                        1 -> (ps ++) `first` loop1 [1] 1 xs
                        _ -> ((ps ++) . (x :)) `first` eventually01s xs



-- | The number of elements of the list ending in @1, 1, 1, 1..@ and the rest of the list
eventually1s :: [Int] -> ([Int], Int)
eventually1s [] = ([], 0)
eventually1s ~(x:xs) = case x of
                         1 -> loop 1 xs
                         _ -> (x :) `first` eventually1s xs
  where
    loop :: Int -> [Int] -> ([Int], Int)
    loop i [] = ([], i)
    loop i ~(y:ys) = case y of
                       1 -> loop (i + 1) ys
                       _ -> ((replicate i 1 ++) . (y :)) `first` eventually1s ys

-- | Count the number of elements in the prefix satisfying the predicate and return the rest of the list
countPrefixOf :: (a -> Bool) -> [a] -> (Int, [a])
countPrefixOf _ [] = (0, [])
countPrefixOf p ~(x:xs) = if p x
                               then (+ 1) `first` countPrefixOf p xs
                               else (0, xs)

-- | Test `countPrefixOf`
testCountPrefixOf :: Int -> Int -> Bool
testCountPrefixOf i j = countPrefixOf id (replicate i True ++ replicate j False) == (i, replicate j False)

-- | Count the number of elements in the suffix satisfying the predicate and return the rest of the list
countSuffixOf :: (a -> Bool) -> [a] -> ([a], Int)
countSuffixOf _ [] = ([], 0)
countSuffixOf p ~(x:xs) = if p x
                             then loop 1 [x] xs
                             else loop 0 [x] xs
  where
    loop i ys [] = (ys, i)
    loop i ys ~(z:zs) = if p z
                           then loop (i + 1) (ys ++ [z]) zs
                           else loop 0 (ys ++ [z]) zs

-- | Test `countSuffixOf`
testCountSuffixOf :: Int -> Int -> Bool
testCountSuffixOf i j = countSuffixOf id (replicate i False ++ replicate j True) == (replicate i False, j)




-- [  x  ]   [  y  ]
-- [0,..,0,1,0,..,0]
middleOne :: Int -> Int -> [Int]
middleOne x y = replicate x 0 ++ [1] ++ replicate y 0


-- | Return the length of the run and the element.
-- For example:
--
-- @
--  λ> `runs` [1,1,2,3,3,3]
--  [(2,1),(1,2),(3,3)]
-- @
--
runs :: Eq a => [a] -> [(Int, a)]
runs = fmap (liftM2 (,) length N.head) . N.group

-- | Tally a `Foldable` of an `Ord`
tallyOrd :: (Foldable t, Ord a) => t a -> Map a Int
tallyOrd = foldl (\mp k -> M.insertWith (+) k 1 mp) mempty

-- | Tally a `Foldable` of `Int`s
tally :: Foldable t => t Int -> IntMap Int
tally = foldl' (\mp k -> IM.insertWith (+) k 1 mp) mempty

-- | Tally `Maxes` of `Int`s
tallyMaxes :: Foldable t => t (Maxes Int) -> IntMap Int
tallyMaxes = foldl (\mp ~(Maxes cnt k) -> IM.insertWith (+) k cnt mp) mempty

-- | Convert @x@ to base @b@
{-# SPECIALISE toBase :: Int -> Int -> [Int] #-}
toBase :: Integral a => a -> a -> [a]
toBase x _
  | x < 0 = error "x < 0"
toBase _ b
  | b < 2 = error "b < 2"
toBase x b =
  if x < b
    then [x]
    else let ~(dv, md) = divMod x b
          in md : toBase dv b


-- | Convert a base-@b@ list of digits to a `Num`
{-# SPECIALISE fromBase :: Int -> [Int] -> Int #-}
fromBase :: Num a => a -> [a] -> a
fromBase b = sum . zipWith (*) (iterate (b *) 1)


-- | Given the number of values, the index of the rotation,
-- and a value to permute (by cycling), perform the permutation.
--
-- λ> simplePerm 5 0 <$> [1..5]
-- [1,2,3,4,5]
--
-- λ> simplePerm 5 1 <$> [1..5]
-- [2,3,4,5,1]
--
-- λ> simplePerm 5 2 <$> [1..5]
-- [3,4,5,1,2]
simplePerm :: Integral a => a -> a -> a -> a
simplePerm n ix x = mod (x - 1 + ix) n + 1


-- | Assert each in list and throw an error with the index if any `False`
asserts :: [Bool] -> IO ()
asserts = mapM_ (uncurry mapper) . zip [(0 :: Int)..]
  where
    mapper i True = print i
    mapper i _    = error $ "Exception at: " ++ show i

-- | `diff` until the list is anywhere decreasing
diffUntilDec :: (Num a, Ord a) => [a] -> [a]
diffUntilDec = until (liftM2 (||) null (or . (zipWith (<) =<< drop 1))) diff

-- https://en.wikipedia.org/wiki/Binomial_transform#Definition
binTransform :: Num a => [a] -> [a]
binTransform = mapOdds negate . diff

-- | `binTransform` @n@ times
binTransformN :: Num a => [a] -> Int -> [a]
binTransformN xs n = nest n binTransform xs

-- | Alternate version of `binTransform`
btrans :: [Int] -> [Int]
btrans xs = [sum [choose n k * (xs !! k) | k <- [0..n]] | n <- [0..length xs-1]]

-- | Alternate version of inverse of `binTransform`
invBtrans :: [Int] -> [Int]
invBtrans xs = [sum [(-1)^(n - k) * choose n k * (xs !! k) | k <- [0..n]] | n <- [0..length xs-1]]

-- | Count the number of elements satisfying the predicate
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = getSum . foldMap (\x -> if p x then 1 else 0)

-- | Map even indices
mapEvens :: (a -> a) -> [a] -> [a]
mapEvens _ [] = []
mapEvens f [x] = [f x]
mapEvens f ~(x:y:zs) = f x : y : mapEvens f zs

-- | Map odd indices
mapOdds :: (a -> a) -> [a] -> [a]
mapOdds _ [] = []
mapOdds _ [x] = [x]
mapOdds f ~(x:y:zs) = x : f y : mapOdds f zs

-- | parityMap f g = mapEvens f . mapOdds g = mapOdds g . mapEvens f
parityMap :: (a -> a) -> (a -> a) -> [a] -> [a]
parityMap _ _ [] = []
parityMap _ g [x] = [g x]
parityMap f g ~(x:y:zs) = f x : g y : parityMap f g zs

-- | Even-index elements
evens :: [a] -> [a]
evens [] = []
evens [x] = [x]
evens ~(x:_:xs) = x : evens xs

-- | Odd-index elements
odds :: [a] -> [a]
odds [] = []
odds [_] = []
odds ~(_:x:xs) = x : odds xs

-- | Apply the function the given number of times
nest n _ _ | n < 0 = error "nest: n < 0"
nest 0 _ x = x
nest n f x = nest (n - 1) f (f x)

-- | Sums of successive pairs of elements
sums :: Num a => [a] -> [a]
sums = zipWith (+) =<< drop 1

-- | The maximum, along with the number of times that it has occurred
data Maxes a = Maxes
  { numMaxes :: !Int
  , getMaxes :: a
  } deriving (Eq, Ord, Show, Functor)

instance Ord a => Semigroup (Maxes a) where
  xs@(~(Maxes cx x)) <> ys@(~(Maxes cy y)) = case compare x y of
                               LT -> ys
                               EQ -> Maxes (cx + cy) x
                               GT -> xs

instance Bounded a => Bounded (Maxes a) where
  minBound = maxes minBound
  maxBound = Maxes maxBound maxBound

instance (Ord a, Bounded a) => Monoid (Maxes a) where
  mempty = minBound

-- | Convert a number to `Maxes`
{-# INLINE maxes #-}
maxes :: a -> Maxes a
maxes = Maxes 1

-- | Convert `Maxes` to a tuple
{-# INLINE unMaxes #-}
unMaxes :: Maxes a -> (Int, a)
unMaxes ~(Maxes c x) = (c, x)


-- | Fold at least one element or fail with `Nothing`.
-- Use that element to build the first @b@.
foldl1Maybe :: Foldable t => (b -> a -> b) -> (a -> b) -> t a -> Maybe b
foldl1Maybe f g = foldl (\x y -> Just $ maybe (g y) (`f` y) x) Nothing


-- | Time (in seconds) that an action takes (does not account for timing overhead)
timed :: IO () -> IO Double
timed action = do
  before <- getMonotonicTime :: IO Double
  action
  after <- getMonotonicTime :: IO Double
  return $ after - before

-- | mapM_ with timing info for each action
mapM_Timed :: (a -> IO ()) -> [a] -> IO ()
mapM_Timed _ [] = return ()
mapM_Timed f ~(x:xs) = do
  duration <- timed $ f x
  putStrLn $ "Took " ++ show duration ++ "s\n"
  mapM_Timed f xs

