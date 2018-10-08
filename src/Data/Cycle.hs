
module Data.Cycle where

{-

-- value :: a -> GappedIndexedCycle a -> (<>) cycles (commutative, associative) -> IndexedCycle a -> Cycle a

-- A cycle of values, represented by a nonempty sequence of values.
newtype Cycle a = Cycle { cycleSeries :: Seq1 a }
  -- { cycleFirstIx :: !Int -- firstIx doesn't seem relevant unless indexed
  -- , cyclePeriod :: !Int -- length of a `Seq` takes constant time, so keeping this here seems unnecessary
  -- , cycleSeries :: !(Seq1 a)
  -- } deriving (Eq, Ord, Show, Read, Generic, NFData)

-- A cycle of `Int`s, represented by a nonempty sequence of `Int`s.
newtype IntCycle = IntCycle { intCycleSeries :: IntSeq1 } -- rotated? we do have log-time rotation
  -- { intCycleFirstIx :: !Int
  -- , intCyclePeriod :: !Int
  -- , intCycleSeries :: !IntSeq1 -- rotated? we do have log-time rotation
  -- }

cycleToIntCycle :: Enum a => Cycle a -> IntCycle
cycleToIntCycle = Cycle . seq1ToIntSeq1 . cycleSeries

intCycleToCycle :: Enum a => IntCycle -> Cycle a
intCycleToCycle = IntCycle . intSeq1ToSeq1 . intCycleSeries


-- rotateCycleL :: Cycle a -> Cycle a
-- rotateCycleL = Cycle . rotateSeq1L . cycleSeries

-- rotateCycleR :: Cycle a -> Cycle a
-- rotateCycleR = Cycle . rotateSeq1R . cycleSeries

-- rotateIntCycleL :: IntCycle -> IntCycle
-- rotateIntCycleL = IntCycle . rotateIntSeq1L . intCycleSeries

-- rotateIntCycleR :: IntCycle -> IntCycle
-- rotateIntCycleR = IntCycle . rotateIntSeq1R . intCycleSeries


-- instance Comonad Cycle where
--   extract = extract . cycleSeries
--   duplicate = Cycle . _ . duplicate . cycleSeries
--   extend f = Cycle . _ . extend (_ f) . cycleSeries

-- extractIntCycle :: IntCycle -> Int

-- | Since an `IntCycle` can only have `Int`s as elements, it can't
-- be duplicated, but we can almost recreate the functionality by
-- allowing the outer layer to be `Cycle`.
duplicateIntCycle :: IntCycle -> Cycle IntCycle
extendIntCycle :: (IntCycle -> Int) -> IntCycle -> IntCycle


-- | Return the first element of a `Cycle` and rotate left once.
unconsCycle :: Cycle a -> (a, Cycle a)
unconsCycle = liftM2 (,) extract rotateCycleL

-- | Return the first element of an `IntCycle` and rotate left once.
unconsIntCycle :: IntCycle -> (Int, IntCycle)
unconsIntCycle = liftM2 (,) extractIntCycle rotateIntCycleL


-- | An indexed cycle, i.e. a `Cycle` with additional context to
-- locate it within a series, including offset and total length.
data IxCycle a = IxCycle
  { ixCycleFirstIx :: !Int
  , ixCycleLen :: !Int -- The number of elements from the first traversed
  -- , ixCyclePeriod :: !Int -- unneeded since easily derived from `Cycle`
  , ixCycle :: !(Cycle a)
  }

-- | An indexed cycle of `Int`s, i.e. a `IntCycle` with additional context to
-- locate it within a series, including offset and total length.
data IntIxCycle = IntIxCycle
  { intIxCycleFirstIx :: !Int
  , intIxCycleLen :: !Int
  -- , intIxCyclePeriod :: !Int -- unneeded since easily derived from `IntCycle`
  , intIxCycle :: !IntCycle
  }

ixCycleToIntIxCycle :: Enum a => IxCycle a -> IntIxCycle
intIxCycleToIxCycle :: Enum a => IntIxCycle -> IxCycle a


-- | An `IxCycle` with gaps

newtype GappedIxCycle a = IxCycle (Int, a)
newtype IntGappedIxCycle a = IxCycle (Int, Int) -- (number of preceding empty spaces, value)

newtype GappedIxCycle a = GappedIxCycle { gappedIxCycle :: Gapped IxCycle a }

instance Semigroup (GappedIxCycle a)


-- | An `IntIxCycle` with gaps
newtype IntGappedIxCycle = IntGappedIxCycle { intGappedIxCycle :: Gapped IxCycle Int }

instance Semigroup (IntGappedIxCycle a)


-- | Either the index of the first remaining gap or an `IxCycle`
fromGappedIxCycle :: GappedIxCycle a -> Either Int (IxCycle a)

-- | Either the index of the first remaining gap or an `IntIxCycle`
fromIntGappedIxCycle :: IntGappedIxCycle -> Either Int (IntIxCycle)

-- | Convert a `GappedIxCycle` to an `IxCycle` by lazily matching on
-- `Just`. If your `GappedIxCycle` can contain `Nothing, you should
-- use `fromGappedIxCycle` instead.
unsafeFromGappedIxCycle :: GappedIxCycle a -> IxCycle a

-- | Convert a `IntGappedIxCycle` to an `IntIxCycle` by lazily matching on
-- `Just`. If your `IntGappedIxCycle` can contain `Nothing, you should
-- use `fromIntGappedIxCycle` instead.
unsafeFromIntGappedIxCycle :: IntGappedIxCycle -> IntIxCycle



-- Combining two Holey Cycles isn't as easy as with lists:
-- - The result `firstIx` is the `min` of the two inputs'
-- - Their lengths are reduced as a result
-- - The new period is the `lcm` (?) of the previous two
-- - length `div` period = whole reps
-- - Combining the series is easier though: we cycle both the appropriate number of times, zipWith (<|>) and collect for the new period.
-- - If Len < Period, the result is a 1-period from the end, or failure if searching for non-trivial cycles


-- Holey f a = f (Maybe a) = MaybeT f a
-- HoleyList a = [Maybe a]

-}
