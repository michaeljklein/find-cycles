
module Data.Map.Utils where

import Data.Map (Map)
import Data.Bifunctor (Bifunctor(..))
import qualified Data.Map as M
import qualified Data.Map.Internal as MI
import Data.IntMap (IntMap, Key)
import qualified Data.IntMap as IM
import qualified Data.IntMap.Internal as IMI


-- | Insert into or update a `Map`
upsert :: Ord k => a -> (a -> a) -> k -> Map k a -> Map k a
upsert x f = M.alter (Just . maybe x f)

-- | Insert into or update an `IntMap`
intUpsert :: a -> (a -> a) -> Int -> IntMap a -> IntMap a
intUpsert x f = IM.alter (Just . maybe x f)

-- | Convert a `Map` to an `IntMap`
mapToIntMap :: Enum k => Map k a -> IntMap a
mapToIntMap = IM.fromDistinctAscList . fmap (first fromEnum) . M.toAscList

-- | Convert an `IntMap` to an `Map`
intMapToMap :: Enum k => IntMap a -> Map k a
intMapToMap = M.fromDistinctAscList . fmap (first toEnum) . IM.toAscList

-- | Lazily match on `MI.Bin`
unsafeFromSingleton :: Map k a -> (k, a)
unsafeFromSingleton ~(MI.Bin _ k x _ _) = (k, x)

-- unsafeUnconsMap :: Map k a -> ((Map k a, Map k a), a)
-- unsafeUnconsMap ~(MI.Bin _ _ x l r) = ((l, r), x)

-- unsafeUnconsIntMap :: IntMap a -> ((IntMap a, IntMap a), a)
-- unsafeUnconsIntMap ~(IMI.Bin _ _ x l r) = ((l, r), x)

foldMapWithKey1 :: Semigroup b => (k -> a -> b) -> Map k a -> b
foldMapWithKey1 _ MI.Tip = error "foldMapWithKey1: Empty Map"
foldMapWithKey1 f ~(MI.Bin _ k x l r) = loop (f k x) l r
  where
    loop y ls rs =
      case ls of
        MI.Tip ->
          case rs of
            MI.Tip -> y
            ~(MI.Bin _ rk rx rl rr) -> y <> loop (f rk rx) rl rr
        ~(MI.Bin _ lk lx ll lr) ->
          case rs of
            MI.Tip -> loop (f lk lx) ll lr <> y
            ~(MI.Bin _ rk rx rl rr) ->
              loop (f lk lx) ll lr <> y <> loop (f rk rx) rl rr

foldIntMapWithKey1 :: Semigroup b => (Key -> a -> b) -> IntMap a -> b
foldIntMapWithKey1 _ IMI.Nil = error "foldIntMapWithKey1: Empty IntMap"
foldIntMapWithKey1 f (IMI.Tip k x) = f k x
foldIntMapWithKey1 f ~(IMI.Bin _ _ l r) = foldIntMapWithKey1 f l <> foldIntMapWithKey1 f r

