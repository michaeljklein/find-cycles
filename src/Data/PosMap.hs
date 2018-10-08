{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.PosMap where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.IntPosMap
import Control.Applicative
import Data.Data
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Control.DeepSeq (NFData(..))
import GHC.Exts
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics
import Data.Map.Utils
import Data.List.Utils

-- | Todo:
--
-- @
--  TODO:
--  - Cycle extraction
--  - Cycle-finding: extractCycle . toPosMap :: (Foldable t, Ord a) => t a -> Maybe (Cycle a) -- Cycle a = NonEmpty a
--  - Testing: vs naive implementation, testing bijections, etc.
--  - Benchmarking: compare to naive, known algorithms. See: https://en.wikipedia.org/wiki/Cycle_detection
--  - Add obvious ways of parallelizing the algorithms
-- @
--
todo :: ()
todo = ()

-- | Note:
--
-- @
--  One of the main attractions of this algorithm is that it's clearly in map-reduce form:
--    Convert all partitions of the series to `PosMap`'s or `IntPosMap`'s (set-parallel)
--    Combine in parallel (ordered-tree-parallel) (See `Semigroup` instances)
--    Consume in parallel (ordered-tree-parallel) (See conversion to `NonEmpty`, `Seq`, `Cycle`)
--
--  Where:
--    set-parallel: Structurally equivalent to mapping over a set with no hidden dependencies
--    ordered-tree-parallel: Structurally equivalent to evaluating an ordered tree with no hidden dependencies (e.g. a rose tree: `Tree`)
--
--  start with a single value, providing a one-layer cycle-decomposition:
--    [x]
--    { x => (0, 0, 0, Cycled 1) }
--
--  if we add an equal value, we get:
--    [x,x]
--    { x => (0, 1, 1, Cycled 2) }
--
--  if we add an unequal value, we get:
--
--  t=[x, y]
--  t={ x => (0, 0, 0, Cycled 1)
--    , y => (1, 1, 0, Cycled 1)
--    }
--
--  t=[x, x, y]
--  t={ x => (0, 1, 1, Cycled 2)
--    , y => (2, 2, 0, Cycled 1)
--    }
--
--  t=[x, x, y, y]
--  t={ x => (0, 1, 1, Cycled 2)
--    , y => (2, 3, 1, Cycled 2)
--    }
--
--  t=[x, x, y, y, x]
--  t={ x => (0, 1, 2, { 0 => (0, 1, 1, Cycled 2), 2 => (2, 2, 0, Cycled 1) } )
--    , y => (2, 3, 1, Cycled 2)
--    }
--
--  t=[x, x, y, y, x, x]
--  t={ x => (0, 5, 3, { 0 => (0, 3, 3, { 0 => (0, 1, 1, Cycled 2), 1 => (3, 3, 0, Cycled 1)}), 2 => (2, 2, Cycled 1) } )
--    , y => (2, 3, 1, Cycled 2)
--    }
--
--  t=[x, x, y, y, x, x, y]
--  t={ x => (0, 5, 3, { 0 => (0, 3, 3, { 0 => (0, 1, 1, Cycled 2), 1 => (3, 3, 0, Cycled 1)}), 2 => (2, 2, Cycled 1) } )
--    , y => (2, 3, 1, { 0 => (2, 3, 1, Cycled 2), 2 => (6, 6, 0, Cycled 1)})
--    }
--
--  Map a (first index, last index, size - 1, Cycled size | IntMap (...)
--
--  - We omit the value inside `Cycled` since it's already in the `PosCxt`
--  - We implement @Cycled  | _@ as `Maybe`
--
--  Diffs are actually (x - y - 1), this makes the minimum key of each intermal map 0 instead of 1.
--
--  Cycled i = replicate i 0
-- @
--
note :: ()
note = ()

-- | To extract a cycle, we get the largest final cycles from each of the subexpressions.
-- We then combine the cycles (that include beginning indices, etc.)
data PosMap (a :: *) = PosMap
  { getPosMap :: Map a PosCxt
  , posMapLen :: !Int
  } deriving (Eq, Ord, Show, Read, Typeable, Data, NFData, Generic)

instance Ord a => Semigroup (PosMap a) where
  PosMap getPosMap1 posMapLen1 <> PosMap getPosMap2 posMapLen2 = PosMap
    { getPosMap = M.unionWith (<>) getPosMap1 getPosMap2
    , posMapLen = posMapLen1 + posMapLen2
    }

instance Ord a => IsList (PosMap a) where
  type Item (PosMap a) = a
  toList = toList . fromPosMap
  fromList =
    fromMaybe (error "IsList PosMap (fromList): empty list") . toPosMap

instance Foldable PosMap where
  foldMap f PosMap{..} = foldMap f $ M.keys getPosMap
  foldr f x PosMap{..} = foldr f x $ M.keys getPosMap

-- | A `PosMap` containing a single element
purePosMap :: a -> PosMap a
purePosMap x = PosMap
  { getPosMap = M.singleton x $ purePosCxt 0
  , posMapLen = 1
  }

-- | Map over the top layer of keys in a `PosMap`
mapPosMap :: Ord b => (a -> b) -> PosMap a -> PosMap b
mapPosMap f PosMap{..} = PosMap { getPosMap = M.mapKeys f getPosMap, .. }

-- | Map over the top layer of keys monotonically in a `PosMap`
mapPosMapMonotonic :: (a -> a) -> PosMap a -> PosMap a
mapPosMapMonotonic f PosMap{..} = PosMap { getPosMap = M.mapKeysMonotonic f getPosMap, .. }


-- | The idea is that we convert each value's `PosCxt` to a list of `Bool`'s,
-- where an element is `True` iff the element in the original list
-- is equal to the given value.
--
-- We convert `True` to the value, `False` to `Nothing`, then we combine the
-- lists by zipping together the `Maybe`'s with @(`<|>`)@.
--
-- Since, for a `PosMap` to be valid all elements must be accounted for,
-- we know that all of the elements in the resulting list must be `Just`.
fromPosMap :: PosMap a -> NonEmpty a
fromPosMap (PosMap _  n)
  | n < 1 = error $ "fromPosMap: non-positive posMapLen: " ++ show n
fromPosMap (PosMap mp _) =
  fromList .
  fmap (\(~(Just x)) -> x) .
  foldl1 (zipWith (<|>)) . fmap (uncurry (fmap runMaybeT . posCxtToGapped)) $
  M.toAscList mp

-- | Convert any non-empty `Foldable` to a `PosMap` by
-- applying `stepPosMap` to `purePosMap` and then each
-- successive element.
toPosMap :: (Foldable t, Ord a) => t a -> Maybe (PosMap a)
toPosMap = foldl1Maybe (flip stepPosMap) purePosMap

-- | Add an element to a `PosMap` by incrementing its length
-- and `upsert`ing the element and position.
stepPosMap :: Ord a => a -> PosMap a -> PosMap a
stepPosMap x PosMap {..} =
  PosMap { getPosMap = upsert (purePosCxt posMapLen) (stepPosCxt posMapLen) x getPosMap
         , posMapLen = posMapLen + 1}



-- | Convert a `PosMap` to an `IntPosMap`
posMapToIntPosMap :: Enum a => PosMap a -> IntPosMap
posMapToIntPosMap PosMap {..} =
  IntPosMap {getIntPosMap = mapToIntMap getPosMap, intPosMapLen = posMapLen}

-- | Convert an `IntPosMap` to a `PosMap`
intPosMapToPosMap :: Enum a => IntPosMap -> PosMap a
intPosMapToPosMap IntPosMap {..} =
  PosMap {getPosMap = intMapToMap getIntPosMap, posMapLen = intPosMapLen}

