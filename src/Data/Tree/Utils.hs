{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}

module Data.Tree.Utils where

import Data.Tree
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty (NonEmpty(..))
import Data.Function
import Data.Bifunctor
import Control.Monad
import Data.List
import Data.Maybe
import Data.Bits
import Data.Semigroup hiding (diff)

-- | Count the number of leaves of a tree
countLeaves :: Tree a -> Int
countLeaves ~(Node _ xs) =
  case xs of
    [] -> 1
    ~(x:xs) -> countLeaves x + getSum (foldMap (Sum . countLeaves) xs)

-- | Convert a non-empty path to a `Tree`
fromPath :: a -> [a] -> Tree a
fromPath x xs =
  Node x $
  case xs of
    [] -> []
    ~(y:ys) -> return $ fromPath y ys

-- | Convert a possibly-empty path to a `Forest`
{-# INLINE fromPathF #-}
fromPathF :: [a] -> Forest a
fromPathF [] = []
fromPathF ~(x:xs) = [fromPath x xs]


-- | The n-th triangular number: @sum [1..n]@
triangular :: Int -> Int
triangular n = unsafeShiftR (n * (n + 1)) 1

-- sum [a..b]
triangularR :: Int -> Int -> Int
triangularR a b = unsafeShiftR (b * (b + 1) - a * (a - 1)) 1

-- λ> (\f n->[f i<$>[i+1..n]|i<-[1..n]]) triangularR 7
-- [[3,6,10,15,21,28],[5,9,14,20,27],[7,12,18,25],[9,15,22],[11,18],[13],[]]
--
-- λ> (\f n->[f i<$>[i+1..n]|i<-[1..n]]) triangularR' 7
-- [[3,6,10,15,21,28],[5,9,14,20,27],[7,12,18,25],[9,15,22],[11,18],[13],[]]
-- triangularR' x y = sum ([x..y] :: [Int])

-- (y * (y + 1)/2) - ((x-1) * x) / 2
-- (y * (y + 1) - x * (x - 1)) / 2



-- | The forest of increasing first-occurrence partitions (beginning with 1) with the given length
-- incParts numberOfDifferences seriesLength seriesSum
incParts :: Int -> Int -> Int -> Forest Int
incParts n _ _ | n < 0 = error $ "incParts: n <  0: " ++ show n
incParts _ m _ | m < 0 = error $ "incParts: m <  0: " ++ show m
incParts _ _ s | s <= 0 = error $ "incParts: s <= 0: " ++ show s

incParts n m _ | n > m = error $ "incParts: n >  m: " ++ show (n, m)
incParts n _ s | n > s = error $ "incParts: n >  s: " ++ show (n, s)
incParts _ m s | m > s = error $ "incParts: m >  s: " ++ show (m, s)

incParts 0 0 0 = []

incParts 1 m s = if m == s
                    then fromPathF $ replicate m 1
                    else error $ "incParts: n == 1 and m /= s: " ++ show (m, s)

incParts n m s = return . Node 1 $ loop 1 (m - 1) (s - 1)
  where
    loop :: Int -> Int -> Int -> Forest Int
    loop i j k =
      case compare minSum k of
        LT ->
          case compare maxSum k of
            LT -> []
            EQ -> boundarySeries i' n (m - n) n -- fromPathF $ [i' .. n] ++ replicate (m - n) n
            GT ->
              if i' <= maxPlaceable
                then Node i' (loop i' (j - 1) (s - i')) :
                     fmap
                       (\x -> Node x $ loop i (j - 1) (s - x))
                       [minPlaceable .. i]
                else fmap
                       (\x -> Node x $ loop i (j - 1) (s - x))
                       [minPlaceable .. maxPlaceable]
        EQ -> boundarySeries i' n (m - n) 1 -- fromPathF $ [i' .. n] ++ replicate (m - n) 1
        GT -> []
      where
        i' = i + 1
        minSum = triangularR i' n + (m - n) -- @(* 1)@
        maxSum = triangularR i' n + (m - n) * n
        minPlaceable = max 1 . min n $ k - maxSum
        maxPlaceable = min i' . max 0 $ k - minSum

-- | The boundary series:
--
-- @z,z..z, x..y, z,z..z@
-- Length is @n@
--
-- i before
-- remainder after
--
-- @
--  [X,z,z,z,z,z,z]
--  [z,X,z,z,z,z,z]
--  [z,z,X,z,z,z,z]
--  [z,z,z,X,z,z,z]
-- @
--
boundarySeries :: Int -> Int -> Int -> Int -> Forest Int
boundarySeries x y z n
  | 1 + y - x > n = error "not enough room for everything"
boundarySeries x y z n =
  if spaceLeft == 0
    then fromPathF [x .. y]
    else [ Node z $ boundarySeries x y z (n - 1)
         , fromPath x $ [x + 1 .. y] ++ replicate spaceLeft z
         ]
  where
    spaceLeft = n - (1 + y - x)


-- Might want to "trim" a Tree:
--
-- Filters out any paths that contain `Nothing`
trimTree :: Tree (Maybe a) -> Maybe (Tree a)
trimTree ~(Node x xs) = (`Node` trimForest xs) <$> x

-- | Trim a forest. See `trimTree`
trimForest :: Forest (Maybe a) -> Forest a
trimForest = mapMaybe trimTree

-- | `cycles` n m = All irreducible cycles of `n` elements of length `m`
--
-- A cycle @cy@ is reducible iff there exists @cy'@, @1 < n@ such that:
--
-- @
--  replicate `n` cy' == cy
-- @
--
cycles :: Int -> Int -> Forest Int
cycles n _
  | n < 1 = error "n < 1"
cycles _ m
  | m < 1 = error "m < 1"
cycles 1 1 = [Node 1 []]
cycles 1 _ = error "n = 1, 1 < m"
cycles n 1 = return <$> [1 .. n]
cycles n m = loopF [] [] . fmap (fmap (+ 1)) $ series n (m - 1) -- cycles n (m - 1)
  where
    -- | @loopF currentListTraversed listOfCycles@
    -- The current values for each cycles (their heads) are
    -- the only values we can't have next
    loopF :: [Int] -> [[Int]] -> Forest Int -> Forest Int
    loopF _ cycles' [] = return <$> ([1 .. n] \\ map (\(~(x:_)) -> x) cycles')
    loopF inits' cycles' xs = loopT inits' cycles' <$> xs

    -- | The mapMaybe filters out all the cycles that are continued
    -- with the current element and rotates each cycles by one
    loopT :: [Int] -> [[Int]] -> Tree Int -> Tree Int
    loopT inits' cycles' ~(Node x xs) =
      Node x $
      loopF
        inits''
        (cycle inits'' :
         mapMaybe
           (\(~(y:ys)) ->
              if x == y
                then Just ys
                else Nothing)
           cycles')
        xs
      where
        inits'' :: [Int]
        inits'' = inits' ++ [x]

-- NEXT:
--   - We want non-cycling then cycling
--   - How does the depth increase as we continue within the cycle (i.e. it should eventually converge)


-- ddiff0s :: Int -> Int -> Int -> Forest Int
-- ddiff0s n _ _ | n < 1 = error $ unwords ["number of 0s (n) < 1:", show n]
-- ddiff0s _ m _ | m < 0 = error $ unwords ["series length (m) < 0:", show m]
-- ddiff0s n m _ | m <= n = error $ unwords ["series length (m) <= number of 0s (n):", show m, "<=", show n]
-- ddiff0s _ _ d | d < 1 = error $ unwords ["number of distinct differences (d) < 1:", show d]
-- ddiff0s n _ d | n <= d = error $ unwords ["number of 0s (n) <= number of distinct differences (d): ", show n, "<=", show d]
-- -- ddiff0s n m d | n + 1 == m = if d == 1
-- --                                 then fromPathF $ replicate n 0 ++ [1]
-- --                                 else error $ unwords ["n + 1 == m, but d /= 1:", show d]

-- ddiff0s n m 1 = fromPathF $ replicate n 0 ++ replicate (m - n) 1


-- | `distinct0s` seems to have predictable polynomial lengths:
--
-- Are these just (length `choose` number_of_0s)?
--
-- λ> (\n->mapM_ print$[getSum.foldMap(Sum. length.paths).distinct0s i<$>[i+1..i+n]|i<-[1..n]]) 12
-- [1,1,1,1,1,1,1,1,1,1,1,1]
-- [1,2,3,4,5,6,7,8,9,10,11,12]
-- [1,3,6,10,15,21,28,36,45,55,66,78]
-- [1,4,10,20,35,56,84,120,165,220,286,364]
-- [1,5,15,35,70,126,210,330,495,715,1001,1365]
-- [1,6,21,56,126,252,462,792,1287,2002,3003,4368]
-- [1,7,28,84,210,462,924,1716,3003,5005,8008,12376]
-- [1,8,36,120,330,792,1716,3432,6435,11440,19448,31824]
-- [1,9,45,165,495,1287,3003,6435,12870,24310,43758,75582]
-- [1,10,55,220,715,2002,5005,11440,24310,48620,92378,167960]
-- [1,11,66,286,1001,3003,8008,19448,43758,92378,184756,352716]
-- [1,12,78,364,1365,4368,12376,31824,75582,167960,352716,705432]
-- (41.30 secs, 21,512,577,928 bytes)
--
-- ((n ==) . count (0 ==)) `filter` distincts 2 m == distinct0s n m
-- distinct0s numberOfZeroes seriesLength
distinct0s :: Int -> Int -> Forest Int
distinct0s n _
  | n < 0 = error $ "n < 0: " ++ show n
distinct0s _ m
  | m < 0 = error $ "m < 0: " ++ show m
distinct0s 0 0 = []
distinct0s 0 m = error $ "0 = n, but 0 < m: " ++ show m
distinct0s 1 1 = [Node 1 []]
distinct0s n m
  | n > m = error $ "n > m: n = " ++ show n ++ ", m = " ++ show m
distinct0s n m
  | n == m = error $ "n == m: " ++ show n
distinct0s n m
  | n == m - 1 = fromPathF $ replicate (m - 1) 0 ++ [1]
distinct0s n m = return . Node 0 $ loop (n - 1) (m - 1)
  where
    loop :: Int -> Int -> Forest Int
    loop 0 j = fromPathF $ replicate j 1
    loop _ 0 = []
    loop i j
      | i + 1 == j = fromPathF $ replicate i 0 ++ [1]
      | otherwise = [Node 0 $ loop (i - 1) (j - 1), Node 1 $ loop i (j - 1)]


-- | @distincts n m =@ the `Tree` of series of length `m` with `n` distinct elements (@[0..n-1]@)
distincts :: Int -> Int -> Forest Int
distincts n _ | n <= 0 = error $ unwords ["distincts: n <= 0:", show n]
distincts _ m | m < 0 = error $ unwords ["distincts: m < 0:", show m]
distincts _ 0 = []
distincts n 1 = return <$> [0..n-1]
distincts 1 m = return . fromPath 0 $ replicate (m - 1) 0
distincts n m = return . Node 0 $ loop 1 1
    -- loop currentDistinctElement currentListSize
  where
    loop i j
      | i == n = series i (m - j)
      | j == m = []
      | m + i == n + j = return $ fromPath i [i+1..n-1]
      | otherwise =
        Node i (loop (i + 1) (j + 1)) :
        ((`Node` loop i (j + 1)) <$> [0 .. i - 1]) -- Either we increment (i) : or not

-- | @series n m =@ the `Tree` of series of length `m` with up to `n` distinct elements (@[0..n-1]@)
series :: Int -> Int -> Forest Int
series n _ | n <= 0 = error $ unwords ["series: n <= 0:", show n]
series _ m | m < 0 = error $ unwords ["series: m < 0:", show m]
series n m = loop m
  where
    loop :: Int -> Forest Int
    loop 0 = []
    loop i = (`Node` loop (i - 1)) <$> [0..n-1]

-- | All paths in a `Tree`
paths :: Tree a -> [[a]]
paths ~(Node x xs) = (x :) <$> concat1 (fmap paths xs)
  where
    concat1 [] = [[]]
    concat1 ys = concat ys

-- | Given a way to convert an initial value to a state, a way to update the state with the new value, and a way to extract a result of whether to keep or not to keep the resulting series,
-- filter a `Tree`
stepFilter ::
     (a -> b) -> (a -> b -> b) -> (b -> Bool) -> Tree a -> Maybe (Tree a)
stepFilter ini stp ext ~(Node x xs) = stepFilterWith stp ext (ini x) x xs

-- | Step-fiter a `Tree`, given an initial state
stepFilter1 :: (a -> b -> b) -> (b -> Bool) -> b -> Tree a -> Maybe (Tree a)
stepFilter1 stp ext y ~(Node x xs) = stepFilterWith stp ext (stp x y) x xs

-- | Step-fiter a piece-wise `Tree`, given an initial state
stepFilterWith ::
     (a -> b -> b) -> (b -> Bool) -> b -> a -> Forest a -> Maybe (Tree a)
stepFilterWith stp ext y x xs =
  case stepFilters1 stp ext y xs of
    [] ->
      if null xs && ext y
        then Just $ Node x xs
        else Nothing
    ys -> Just $ Node x ys

-- | Step-fiter a `Forest`, given an initial state function
stepFilters :: (a -> b) -> (a -> b -> b) -> (b -> Bool) -> Forest a -> Forest a
stepFilters = fmap (fmap mapMaybe) . stepFilter

-- | Step-fiter a `Forest`, given an initial state
stepFilters1 :: (a -> b -> b) -> (b -> Bool) -> b -> Forest a -> Forest a
stepFilters1 = fmap (fmap mapMaybe) . stepFilter1

-- | Map over all sub-`Forest`s in a `Tree`
mapsTree :: (Forest a -> Forest a) -> Tree a -> Tree a
mapsTree f ~(Node x xs) = Node x . f $ mapsTree f <$> xs

-- | Replace each node in a tree with the the `NonEmpty` list consisting of the original node, followed by the first path after that node.
--
-- When using `drawTree` or `drawTreeU`, this can be used to compress the representation, especially when there is little branching.
firstForest :: Tree a -> Tree (NonEmpty a)
firstForest ~(Node x xs) =
  case xs of
    [] -> Node [x] []
    ~(y:ys) ->
      case firstForest y of
        ~(Node (z :| zs) ws) -> Node (x :| (z : zs)) $ ws ++ fmap firstForest ys

deriving instance Ord a => Ord (Tree a)

-- | A `Forest` of all series with elements in @1..input@
{-# SPECIALISE fseries :: Int -> Forest Int #-}
fseries :: Enum a => a -> Forest a
fseries n = unfoldForest (, [toEnum 1..n]) [toEnum 1..n]

-- | Scan along a `Tree` with a stepper and converter
stepTree :: (a -> b -> b) -> (a -> b) -> Tree a -> Tree b
stepTree stp cnv ~(Node x xs) =
  let x' = cnv x
   in Node x' $ stepForest stp cnv x' xs

-- | Scan along a `Tree` with a stepper, converter, and initial scan state
stepTree1 :: (a -> b -> b) -> (a -> b) -> b -> Tree a -> Tree b
stepTree1 stp cnv x ~(Node y ys) =
  let y' = stp y x
   in Node y' $ stepForest stp cnv y' ys

-- | Scan along a `Forest` with a stepper, converter, and initial scan state
stepForest :: (a -> b -> b) -> (a -> b) -> b -> Forest a -> Forest b
stepForest = fmap (fmap fmap) . stepTree1

-- | `stepTree`, returning a list of successive states
stepTreeList :: (a -> b -> b) -> (a -> b) -> Tree a -> Tree ([a], b)
stepTreeList stp cnv ~(Node x xs) =
  let x' = cnv x
   in Node ([x], x') $ fmap (first (x :)) <$> stepForestList stp cnv x' xs

-- | Scan along a `Tree` with a stepper, converter, and initial scan state
stepTreeList1 :: (a -> b -> b) -> (a -> b) -> b -> Tree a -> Tree ([a], b)
stepTreeList1 stp cnv x ~(Node y ys) =
  let y' = stp y x
   in Node ([y], y') $ fmap (first (y :)) <$> stepForestList stp cnv y' ys

-- | Scan along a `Forest` with a stepper, converter, and initial scan state
stepForestList :: (a -> b -> b) -> (a -> b) -> b -> Forest a -> Forest ([a], b)
stepForestList = fmap (fmap fmap) . stepTreeList1


-- | Equivalent to:
--
-- @
--  unzip . map (\(Node x xs) -> (x, xs))
-- @
--
unzipForest :: Forest a -> ([a], [Forest a])
unzipForest = foldr (\(~(Node x ys)) (xs, yss) -> (x : xs, ys : yss)) ([], [])


-- | Take up to the given depth
takeDepth :: Int -> Tree a -> Tree a
takeDepth 0 ~(Node x _) = Node x []
takeDepth n ~(Node x xs) = Node x $ takeDepths (n - 1) xs

-- | Take up to the given depth
takeDepths :: Int -> Forest a -> Forest a
takeDepths = fmap . takeDepth

-- | Maximum of a tree, using a comparison function
maxTreeOn_ :: Ord b => (a -> b) -> Tree a -> [a]
maxTreeOn_ f ~(Node x xs) = x : maxTreeOn_ f (maxForestOn_ f xs)

-- | The maximum of a forest on a given value, returning that value.
--
-- This implementation assumes that computing the given function
-- is expensive and only computes it once.
maxForestOn :: Ord b => (a -> b) -> Forest a -> Tree (b, a)
maxForestOn f =
  uncurry Node .
  bimap
    (maximumBy (compare `on` fst) . fmap (first f . join (,)))
    (maxForestOn f <$>) .
  unzipForest

-- | The maximum of a forest on a given value.
--
-- This implementation assumes that computing the given function
-- is cheap.
maxForestOn_ :: Ord b => (a -> b) -> Forest a -> Tree a
maxForestOn_ f =
  uncurry Node .
  bimap
    (maximumBy (compare `on` f))
    (maxForestOn_ f <$>) .
  unzipForest


-- | From: https://github.com/haskell/containers/pull/344/files
--
-- Neat 2-dimensional drawing of a tree. Unicode paths
drawTreeU :: Tree String -> String
drawTreeU  = unlines . drawU

-- | More compact `drawTreeU`
drawTreeU' :: Tree String -> String
drawTreeU'  = unlines . drawU'

-- | Neat 2-dimensional drawing of a forest. Unicode paths
drawForestU :: Forest String -> String
drawForestU  = unlines . map drawTreeU

-- | Draw a tree with spaces
drawU :: Tree String -> [String]
drawU (Node x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        shift "  " "  " (drawU t)
    drawSubTrees (t:ts) =
        shift "  " "  " (drawU t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)

-- | Draw a tree with unicode
drawU' :: Tree String -> [String]
drawU' (Node x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        shift "\x2514\x2500" "  " (drawU' t)
    drawSubTrees (t:ts) =
        shift "\x251c\x2500" "\x2502 " (drawU' t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)


