
module Data.Sequence1 where

import Data.Sequence

-- | A non-empty `Seq`, with a strict head and tail
data Seq1 a = Seq1 { seq1Head :: !a, seq1Tail :: !(Seq a) }

{-

instance IsList (Seq1 a)

instance Foldable Seq1

rotateSeq1L :: Seq1 a -> Seq1 a
rotateSeq1R :: Seq1 a -> Seq1 a

rotateSeq1 :: Int -> Seq1 a -> Seq1 a

-- | Cyclic indexing
indexSeq1 :: Int -> Seq1 a -> a

seq1ToNonEmpty :: Seq1 a -> NonEmpty a

seq1ToSeq :: Seq1 a -> Seq a
seqToSeq1 :: Seq a -> Maybe (Seq1 a)

-- | A non-empty `Seq` of `Int`s
data IntSeq1 = IntSeq1 { intSeq1Head :: !Int, intSeq1Tail :: !(Seq Int) }

instance IsList IntSeq1

rotateIntSeq1L :: IntSeq1 -> IntSeq1
rotateIntSeq1R :: IntSeq1 -> IntSeq1

rotateIntSeq1 :: Int -> IntSeq1 -> IntSeq1

-- | Cyclic indexing
indexIntSeq1 :: Int -> IntSeq1 -> Int

intSeq1ToNonEmpty :: Enum a => IntSeq1 -> NonEmpty a

intSeq1ToSeq :: Enum a => IntSeq1 -> Seq a
seqToIntSeq1 :: Enum a => Seq a -> Maybe IntSeq1

mapIntSeq1 :: (Int -> Int) -> IntSeq1 -> IntSeq1

-- concatMapIntSeq1 :: (Int -> IntSeq1) -> IntSeq1 -> IntSeq1

-- mapMaybeIntSeq1 :: (Int -> Maybe Int) -> IntSeq1 -> IntSeq1

-}
