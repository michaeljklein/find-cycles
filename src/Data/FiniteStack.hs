
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FiniteStack
-- Copyright   :  (C) 2018 Michael J. Klein
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  lambdamichael@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- It's an interesting idea, but I don't really have much of a use
-- for compact finite stacks forming n-ary trees using `MVector`'s.
----------------------------------------------------------------------------
module Data.FiniteStack () where

{-

-- | Finite stacks. These can be used to define n-ary trees that can be appended in log_n time.
--
-- Alternatively, if something other than `FiniteStack` is used to recurse, such as `FreeSemigroup`,
-- we get a layer of granularity above individual values.
data FiniteStack (n :: Nat) a = FiniteStack
  { finiteStackLen :: {-# UNBOX #-} !Int -- ^ Current top value index
  , finiteStack    :: forall s. ST s (MVector s a) -- ^ `MVector` that implements the stack
  }

finiteStackSize :: FiniteStack n a -> Proxy n
finiteStackSize _ = Proxy

finiteStackSz :: KnownNat n => FiniteStack n a -> Int
finiteStackSz = fromInteger . natVal . finiteStackSize


newFiniteStack :: (MVector v a, KnownNat n) => a -> FiniteStack n a
newFiniteStack = proxyNewFiniteStack Proxy

proxyNewFiniteStack :: (MVector v a, KnownNat n) => proxy n -> a -> FiniteStack n a
proxyNewFiniteStack prxy !x = FiniteStack
  { finiteStackIx = 0
  , finiteStack = do
    v <- unsafeNew . fromInteger $ natVal prxy
    unsafeWrite v 0 x
    return v
  }


instance (MVector v a, KnownNat n) => IsList (FiniteStack n a) where
  fromList = liftM2 fromListN length id

  fromListN = proxyFromListN Proxy

  toList FiniteStack{..} = _ $
    finiteStack >>= SM.toList . mstream . unsafeTake (finiteStackIx + 1)

proxyFromListN :: (MVector v a, KnownNat n) => proxy n -> Int -> [a] -> FiniteStack n a
proxyFromListN prxy n xs
  | n < 1  = error $ "proxyFromListN: non-positive size: " ++ show n
  | n > sz = error $ "proxyFromListN: given size is greater than maximum: " ++ show n ++ " > " ++ show sz
  | otherwise = FiniteStack
    { finiteStackIx = n - 1
    , finiteStack = do
      v <- unsafeNew sz
      v `fill` SM.fromListN n xs
      return v
    }
  where
    sz = fromInteger $ natVal prxy

newtype These2 a = These2 { runThese2 :: These a a }

appendFiniteStacks :: (MVector v a, KnownNat n) => FiniteStack n a -> FiniteStack n a -> These2 (FiniteStack n a)

Assume both are not full, then the result can be:
  One not full
  One full
  One full and one not full

Unless their lengths are greater than the max size (resulting in the third case):
  Copy all of smaller to end of bigger

In the last case:
  Copy smaller up to end of bigger
  Copy rest of smaller to memo pad -> is new result
  Smaller is new memo pad


The idea is that we have a fast possibly-empty collection of full FiniteStack's and a single non-full FiniteStack.

To append a value, we append to the front FiniteStack and add to the collection of fulls if full.

To mappend two, we mappend their full FiniteStack's and use appendFiniteStacks to combine their heads, with a possible
  single new full FiniteStack to be added to the collection.


How about:
  We allow the FiniteStack's to be empty
  We recurse to use this FiniteStack method for our "fast possibly-empty collection"

newtype FiniteStacks n a = (FiniteStack n a, FiniteStacks n (FiniteStack n a))



-- f a = (g a, f (g a))
-- f = g :*: (f :.: g)


-- fill :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Stream m a -> m (v (PrimState m) a)



-- mstream :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Stream m a

-- Conversions
-- SM.toList :: Monad m => Stream m a -> m [a] Source#

-- Convert a Stream to a list

-- SM.fromList :: Monad m => [a] -> Stream m a Source#

-- Convert a list to a Stream

-- SM.fromListN :: Monad m => Int -> [a] -> Stream m a

-- | `Nothing` if full
-- pushFiniteStack :: (MVector v a, KnownNat n) => a -> FiniteStack v n a -> Maybe (FiniteStack v n a)
-- pushFiniteStack x fs@FiniteStack{..} | _

-- instance (MVector v a, KnownNat n) => _

-- unsafeNew :: (PrimMonad m, MVector v a) => Int -> m (v (PrimState m) a) Source#

-- unsafeWrite :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()

-- Create a mutable vector of the given length. The memory is not initialized.


-}

