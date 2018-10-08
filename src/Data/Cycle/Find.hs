{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

module Data.Cycle.Find where

import GHC.Exts
import Data.List
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

-- | Get the initial state from the first element of a `Foldable` or return `Nothing`
foldl1Maybe :: Foldable t => (b -> a -> b) -> (a -> b) -> t a -> Maybe b
foldl1Maybe f g = foldl (\x y -> Just $ maybe (g y) (`f` y) x) Nothing

-- | Goal:
--
-- @
--  Goal:
--    Given a list, function, series, etc.
--    1. If there exists xs /= input | and (zipWith (==) (cycle xs) input)
--       Output the shortest, else Nothing.
--    2. If there exists xs | exists (0 <= n) | and (zipWith (==) (cycle xs) (drop n input) && n + 2 * length xs <= length input
--       (i.e. it eventually cycles, and the cycle repeats fully at least once)
--       Output the shortest, else Nothing.
--
--  - If we end at anything other than a single value repeated, we recurse.
--
--  So:
--    Single value repeated n times
--    Values repeated some number of times, recursed on their cycles
--
--  Ahh!
--    I was thinking we didn't need to store the entire previous list, we don't!
--      We should just store a seq for the current cycle, since fast cons and fast single-element rotation.
--
--  When a cycle ends, if there were no previous non-equal values, then we'll need to recurse.
--
--  - If we keep a current-label state and add a label to each value, all value sets after the first will be a range [0..n]
--    * Since the value sets are monotonically decreasing in size, we can use a single allocation to represent all of the maps
-- @
--
goal :: ()
goal = ()

-- | A representation of a finite cycle (the list should never be infinite)
data Cycle a = Cycle { cycleLen :: Int, cycleSeries :: [a] }

data Cyclic a = Cyclic { cyclicLen :: Int, cyclicCycle :: Cycle a }


-- diff :: Num a => [a] -> [a]
-- diff = zipWith(-) =<< tail

-- | `Just` the cycle that makes up the input or `Nothing`
--
-- @
--  cyclic (take _ $ cycle xs) = Just xs
--  cyclic _ = Nothing
-- @
--
cyclic :: Ord a => [a] -> Maybe [a]
cyclic xs = listToMaybe . catMaybes $ boolMaybe (isCycleOf xs) <$> inits xs

-- | `Just` if the predicate returns `True`
boolMaybe :: (a -> Bool) -> a -> Maybe a
boolMaybe p x = if p x
                   then Just x
                   else Nothing

-- | Is the second list a cycle of the first?
isCycleOf :: Eq a => [a] -> [a] -> Bool
isCycleOf _  [] = False
isCycleOf xs ys =
  maybe False or $
    zipWithM (\x ~(twoCycles', y) -> if x == y then Just twoCycles' else Nothing)
    xs
    (twoCycles ys)

-- | (have we reached two cycles?, value)
twoCycles :: [a] -> [(Bool, a)]
twoCycles xs =
  concat
    [ mark False xs
    , mark False (init xs)
    , mark True [last xs]
    , cycle (mark True xs)
    ]
  where
    mark b = map (b, )

-- | Indexed differences between locations of an element
indexDiffs :: Ord a => [a] -> Map a (Int, [Int])
indexDiffs = loop 0 mempty
  where
    loop _ mp [] = mp
    loop i mp ~(x:xs) = loop (i + 1) (M.alter alterer x mp) xs
      where
        alterer :: Maybe (Int, [Int]) -> Maybe (Int, [Int])
        alterer =
          fmap Just . maybe (i, []) $ \(~(iLast, diffs')) ->
            (i, (i - iLast) : diffs')

-- | `indexDiffs` specialized to `Int`
intIndexDiffs :: [Int] -> IntMap (Indexed [Int])
intIndexDiffs = loop 0 mempty
  where
    loop _ mp [] = mp
    loop i mp ~(x:xs) = loop (i + 1) (IM.alter alterer x mp) xs
      where
        alterer :: Maybe (Indexed [Int]) -> Maybe (Indexed [Int])
        alterer =
          fmap Just . maybe (Indexed i []) $ \(~(Indexed iLast diffs')) ->
            Indexed i $ (i - iLast) : diffs'

-- | The indices of a value in a list
indices :: Eq a => a -> [a] -> [Int]
indices x = fmap fst . filter ((== x) . snd) . zip [0 ..]



-- ok, so all of the individual values must be cyclic so:
--   isX :: Eq a => a -> a -> Maybe a
--   isX x y = if x == y then Just x else y

--   fmap . isX :: Eq a => a -> [a] -> [Maybe a]
--   cyclic . fmap . isX :: a -> [a] -> Maybe [Maybe a]

-- gaps between individual values must be cyclic:
--   cyclic xs iff all (cyclic . (`indices` xs)) (nub $ sort xs)

--   cyclic xs = mapM (cyclic . diff . (`indices` xs)) ((nub . sort) xs)
--   indices :: Eq a => a -> [a] -> [Int]
--   indices x = loop 0
--     where
--       loop _ [] = []
--       loop i ~(y:ys) = if x == y
--                           then i : loop (i + 1) ys
--                           else     loop (i + 1) ys

-- eventuallyCyclic :: Ord a => [a] -> Maybe (Int, [a]) -- (length of non-cyclic prefix)


-- data Cycled a = Cycled
--   { runCycled :: Map a ValueState
--   , cycle :: IxF Seq1 a
--   , cycleLen :: Int
--   , cycleReps :: Int
--   }

-- data IntCycled = IntCycled
--   { runCycled :: IntMap ValueState
--   , currentCycle :: IntSeq1
--   , currentCycleLen :: Int
--   , currentCycleReps :: Int
--   }

-- ValueState = ValueState
--   { firstPos :: {-# UNPACK #-} !Int
--   , lastPos  :: {-# UNPACK #-} !Int
--   , locPositions :: IntCycled
--   }


-- stepCycled :: Ord a => Cycled a -> a -> Cycled a


-- Indexed

-- | An `Int`-indexed value
data Indexed a = Indexed
  { indexOf :: {-# UNPACK #-} !Int
  , indexed :: a
  } deriving (Eq, Ord, Show, Read, Functor)

instance Foldable Indexed where
  foldr f x ~(Indexed _ y) = f y x
  foldMap f ~(Indexed _ y) = f y

instance Traversable Indexed where
  traverse f ~(Indexed i x) = Indexed i <$> f x
  sequenceA ~(Indexed i x) = Indexed i <$> x

-- | `uncurry` for `Indexed`
uncurryi :: (Int -> a -> b) -> Indexed a -> b
uncurryi f ~(Indexed i x) = f i x

-- | `curry` for `Indexed`
curryi :: (Indexed a -> b) -> Int -> a -> b
curryi f !i x = f $ Indexed i x

-- -- | Convert an indexed fold to one that is not indexed, but also returns the length of the input.
-- ifoldLen :: Fold (Indexed a) b -> Fold a (Indexed b)
-- ifoldLen f = (`purely` f) $ \stp ini ext -> Fold (\(~(Indexed i x)) -> Indexed (i + 1) . stp x) (Indexed 0 ini) (fmap ext)

-- -- | Convert an indexed monadic fold to one that is not indexed, but also returns the length of the input.
-- ifoldLenM :: Monad m => FoldM m (Indexed a) b -> FoldM m a (Indexed b)
-- ifoldLenM f = (`impurely` f) $ \stp ini ext -> FoldM (\(~(Indexed i x)) -> fmap (Indexed (i + 1)) . stp x) (Indexed 0 <$> ini) (mapM ext)



--stepPositionDiffMap :: Ord a => Int -> a -> Map a (Int, [Int]) -> Map a (Int, [Int])
--stepPositionDiffMap !i x = M.alter (Just . maybe (Indexed i []) (fmap (x :)))

--positionDiffMap :: Ord a => Fold a (Indexed (Map a (Indexed [Int])))
--positionDiffMap = ifoldLen (Fold (flip $ uncurryi stepPositionDiffMap) mempty id)

--newtype PosDiffs a = PosDiffs { getPosDiffs :: Map a (Indexed [Int]) }

--newtype IntPosDiffs = IntPosDiffs { getIntPosDiffs :: IntMap (Indexed [Int] }

--So, when we recurse, we start with a layer of a's (PosDiffs) and then all subsequent layers are IntPosDiffs.

--- Should we also include a list of values by first occurrence?
--  This could make it easier to operate on PosDiffs by allowing us to traverse the list.

--- I think it should be an exceptional Cofree,
--  where the exception is that cycles to merge up have been found (monads propagate up).
--  So the normal layer will look something like:
--  Except resultFound (IntMap (Indexed ([Int], Except resultFound (IntMap ..


--When we insert a new value, we update the top-level map, propagate the new differences/etc down the comonad, propagating up any solutions to re-merge and continue with.

--I believe this is a form of dynamic programming.

--(Extend to Reals by binning or by changing how we recognize cycles (e.g. if all values within 1% of each other, make bin for them. OR allow pseudocycles))


---- pseudocycles:
----   (a+b+c+d+e+f+)+
----
---- (1+0+)+
----   10
----   101010
----   110
----   110110110
----   1100
----   1100110011001100

--I think you find pseudocycles by tallying the list (tally = fmap (first length . join (,)) . group) and then the cycles of that list without multiplicities are the pseudocycles.



--po-cycles (partial-order cycles):
--  the cycles of the comparisions between successive elements.

-- | Non-empty cycles, composed of an index of rotation and a rotated state.
-- Operations are provided to:
-- - extract/build from index/state
-- - map over each
-- - rotations (succIxF, predIxF, seekIxF)
-- - relative indexing
--
-- Example instances:
-- - cyclically indexed list, vector, etc.
-- - _

--class CycleIx (f :: *) a where
--  data IxState f a :: *
--  data IxF f a :: *

--  -- | Half of an isomophism:
--  --
--  -- @
--  --  `uncurry` `toIxF` :: (`Int`, `IxState` f a) -> `IxF` f a
--  --  `liftM2` (,) `ixF` `ixState` :: `IxF` f a -> (`Int`, `IxState` f a)
--  -- @
--  toIxF :: Int -> IxState f a -> IxF f a
--  ixF :: IxF f a -> Int
--  ixState :: IxF f a -> IxState f a

--  mapIxF :: (Int -> Int) -> IxF f a -> IxF f a
--  mapIxF f = liftM2 toIxF (f . ixF) ixState

--  mapIxState :: (IxState f a -> IxState f a) -> IxF f a -> IxF f a
--  mapIxState f = liftM2 toIxF ixF (f . ixState)

--  bimapIxF :: (Int -> Int) -> (IxState f a -> IxState f a) -> IxF f a -> IxF f a
--  bimapIxF f g = mapIxF f . mapIxState g

--  succIxF :: IxF f a -> IxF f a
--  succIxF = seekIxF 1

--  predIxF :: IxF f a -> IxF f a
--  predIxF = seekIxF (-1)

--  seekIxF :: Int -> IxF f a -> IxF f a
--  seekIxF i = case compare i 0 of
--                LT -> nest (negate i) predIxF
--                EQ -> id
--                GT -> nest i succF

--  headIxF :: IxF f a -> a
--  headIxF = indexIxF 0

--  indexIxF :: Int -> IxF f a -> a
--  indexIxF i = headIxF . seekIxF i

--  buildIxF :: a -> IxF f a

--  snocIxF :: a -> IxF f a -> IxF f a

--  foldableIxF :: Foldable t => t a -> Maybe (IxF f a)
--  foldableIxF = foldl1Maybe (flip snocIxF) buildIxF
--    where
--      foldl1Maybe f g = foldl (\x y -> Just $ maybe (g y) (`f` y) x) Nothing


--data IxF f a = IxF { _ :: !Int, _ :: IxState f a }

