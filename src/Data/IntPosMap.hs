{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.IntPosMap where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.Data
import Data.Function
import Data.Functor.Identity
import Data.IntMap.Strict (IntMap)
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Map.Utils
import Data.Maybe
import Data.Semigroup hiding (diff)
import Data.Semigroup.Free
import Data.Tuple
import GHC.Exts
import GHC.Generics
import qualified Data.IntMap.Strict as IM
import qualified Data.List.NonEmpty as N
import qualified Data.Map as M

import Text.Printf
import Data.Tree
import GHC.Clock

import Data.BStream
import Data.Pair

import Control.Monad.Trans.State

import Math.Combinatorics.Exact.Binomial
import Data.Bits

import Data.List.Utils
import Data.Tree.Utils

import Test.QuickCheck


-- IMPORTANT:
--   all insertions of new diffs are insertions of new maximums!


maxDiffSeries :: [Int]
maxDiffSeries = 0 : loop 0 1
  where
    loop :: Int -> Int -> [Int]
    loop 0 n = 0 : loop n (n + 1)
    loop i n = 1 : loop (i - 1) n

-- stepMaxDiffSeries :: (Int -> a -> a) -> (Int -> a) -> [a]
-- stepMaxDiffSeries stp cnv =

-- | Scan, stepping an `IntPosMap` along a non-empty list
scanIntPosMap :: Enum a => [a] -> [IntPosMap]
scanIntPosMap ~(x:xs) =
  scanl (\pm y -> stepIntPosMap (fromEnum y) pm) (pureIntPosMap (fromEnum x)) xs

-- | also return current list
scanIntPosMapL :: Enum a => [a] -> [([a], IntPosMap)]
scanIntPosMapL = liftM2 zip (tail . inits) scanIntPosMap


-- | The depths of a list as it cycles, after an initial non-cycling section
intPosMapThenCycles :: [Int] -> [Int] -> [Int]
intPosMapThenCycles [] ys = intPosMapCycles ys
intPosMapThenCycles ~(x:xs) ys =
  fmap intPosMapDepth . scanl (flip stepIntPosMap) (pureIntPosMap x) $
  xs ++ cycle ys

-- | The depths of a list as it cycles
intPosMapCycles :: [Int] -> [Int]
intPosMapCycles xs =
  case cycle xs of
    ~(y:ys) ->
      intPosMapDepth <$> scanl (flip stepIntPosMap) (pureIntPosMap y) ys

-- | Only output depths of full cycles
intPosMapFullCycles :: [Int] -> [Int]
intPosMapFullCycles xs = intPosMapDepth <$> scanl (foldl (flip stepIntPosMap)) (fromList xs) (repeat xs)


-- Also specify number of distinct differences

-- It turns out that when we know the number of distinct differences and this new computation,
--   we know the multiplicities of the depths since rearranging differences for a single value preserves its depth.

-- Therefore, the distinct differences must be strictly increasing in size (for their first occurences)

-- the minimum difference is 1
-- the absolute maximum difference is (0 `min` (m - 1))
-- the largest difference we can have "now" is: 1 + m - n


-- OK, so we know:
-- - If all differences between indices are `(1 <)`, we can subtract `(1)` from
--   all indices while preserving the depth
-- - If one differences occurrs first before another does, we can swap the differences just like we swap labels

-- This means that we can count the multiplicities of depths where:
-- - Number of values
-- - Nubmer of distinct differences between indices of values
-- - Length of the resulting series

-- are all given.
-- Furthermore, the minimum difference must be `(1)` and the first occurrences of
-- each difference must occur in increasing order.

-- What about other scaling?
--   e.g. what if the differences are:
--     [1,1,3,3,3,1,1,1]
--   I believe this must have the same depth as:
--     [1,1,2,2,2,1,1,1]
-- And indeed it does. While we can scale multiple differences at once,
--   it's not very likely that or predictable whether we can modify the
--   differences while preserving the resulting length.


distinctDepths :: Int -> Int -> IntMap Int
distinctDepths =
  fmap (tally . concatMap (fmap (intPosMapDepth . fromList) . paths)) .
  distincts

distinctDepths' :: Int -> Int -> IntMap Int
distinctDepths' =
  fmap (tallyMaxes . concatMap (fmap (intPosMapDepths . fromList) . paths)) .
  distincts

distinctKeyDepths :: Int -> Int -> Map (IntMap Int) Int
distinctKeyDepths =
  fmap (tallyOrd . concatMap (fmap (intPosMapKeyDepths . fromList) . paths)) .
  distincts

distinct0Depths :: Int -> Int -> IntMap Int
distinct0Depths =
  fmap
    (tally . concatMap (fmap ((IM.! 0) . intPosMapKeyDepths . fromList) . paths)) .
  distinct0s

-- 2s

d0d2s_0 :: Int -> [Int]
d0d2s_0 n = (head.transpose.map(map snd.toList.distinct0Depths 2))[3..3+n-1]

d0d2s_0_18 :: [Int]
d0d2s_0_18 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]

d0d2fs_0 :: Int -> [Int]
d0d2fs_0 n = [1..n] -- scanl (+) 1 $ cycle [1]

prop_d0d2s_0 :: Bool
prop_d0d2s_0 = d0d2s_0_18 == d0d2s_0 18 && liftM2 (==) d0d2s_0 d0d2fs_0 18


-- 3s

d0d3s_0 :: Int -> [Int]
d0d3s_0 n = ((!! 0).transpose.map(map snd.toList.distinct0Depths 3))[4..4+n-1]

d0d3s_0_22 :: [Int]
d0d3s_0_22 = [1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11]

d0d3fs_0 :: Int -> [Int]
d0d3fs_0 n = take n . scanl (+) 1 $ cycle [0, 1]

prop_d0d3s_0 :: Bool
prop_d0d3s_0 = d0d3s_0_22 == d0d3s_0 22 && d0d3s_0 22 == d0d3fs_0 22

d0d3s_1 :: Int -> [Int]
d0d3s_1 n = ((!! 1).transpose.map(map snd.toList.distinct0Depths 3))[4..4+n]

d0d3s_1_21 :: [Int]
d0d3s_1_21 = [2,4,8,12,18,24,32,40,50,60,72,84,98,112,128,144,162,180,200,220,242]

d0d3fs_1 :: Int -> [Int]
d0d3fs_1 n = take n . scanl (+) 2 . scanl (+) 2 $ cycle [2, 0]

prop_d0d3s_1 :: Bool
prop_d0d3s_1 = d0d3s_1_21 == d0d3s_1 21 && d0d3s_1 21 == d0d3fs_1 21

-- 4s

d0d4s_0 :: Int -> [Int]
d0d4s_0 n = ((!! 0).transpose.map(map snd.toList.distinct0Depths 4))[5..5+n-1]

d0d4s_0_21 :: [Int]
d0d4s_0_21 = [1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7]

d0d4fs_0 :: Int -> [Int]
d0d4fs_0 n = take n . scanl (+) 1 $ cycle [0, 0, 1]

prop_d0d4s_0 :: Bool
prop_d0d4s_0 = d0d4s_0_21 == d0d4s_0 21 && d0d4s_0 21 == d0d4fs_0 21

d0d4s_1 :: Int -> [Int]
d0d4s_1 n = ((!! 1).transpose.map(map snd.toList.distinct0Depths 4))[5..5+n]

d0d4s_1_20 :: [Int]
d0d4s_1_20 = [2,6,14,26,44,68,100,140,190,250,322,406,504,616,744,888,1050,1230,1430,1650]

d0d4fs_1 :: Int -> [Int]
d0d4fs_1 n = take n . scanl (+) 2 . scanl (+) 4 . scanl (+) 4 $ cycle [0, 2]

prop_d0d4s_1 :: Bool
prop_d0d4s_1 = d0d4s_1_20 == d0d4s_1 20 && d0d4s_1 20 == d0d4fs_1 20

d0d4s_2 :: Int -> [Int]
d0d4s_2 n = ((!! 2).transpose.map(map snd.toList.distinct0Depths 4))[5..5+n]

d0d4s_2_30 :: [Int]
d0d4s_2_30 = [1,3,4,7,10,13,17,22,26,32,38,44,51,59,66,75,84,93,103,114,124,136,148,160,173,187,200,215,230,245]

d0d4fs_2 :: Int -> [Int]
d0d4fs_2 n = take n . scanl (+) 1 . scanl (+) 2 $ cycle [-1, 2, 0, 0, 1, 1]

prop_d0d4s_2 :: Bool
prop_d0d4s_2 = d0d4s_2_30 == d0d4s_2 30 && d0d4s_2 30 == d0d4fs_2 30

-- 5s

d0d5s_0 :: Int -> [Int]
d0d5s_0 n = ((!! 0).transpose.map(map snd.toList.distinct0Depths 5))[6..6+n-1]

d0d5s_0_20 :: [Int]
d0d5s_0_20 = [1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5]

d0d5fs_0 :: Int -> [Int]
d0d5fs_0 n = take n . scanl (+) 1 $ cycle [0, 0, 0, 1]

prop_d0d5s_0 :: Bool
prop_d0d5s_0 = d0d5s_0_20 == d0d5s_0 20 && d0d5s_0 20 == d0d5fs_0 20

d0d5s_1 :: Int -> [Int]
d0d5s_1 n = ((!! 1).transpose.map(map snd.toList.distinct0Depths 5))[6..6+n]

d0d5s_0_30 :: [Int]
d0d5s_0_30 = [1,5,15,36,72,130,217,340,510,735,1027,1398,1861,2429,3119,3945,4925,6077,7420,8973,10759,12798,15114,17731,20674,23968,27642,31722,36238,41220]

d0d5s_2 :: Int -> [Int]
d0d5s_2 n = ((!! 2).transpose.map(map snd.toList.distinct0Depths 5))[6..6+n]

d0d5IO :: IO ()
d0d5IO = mapM_Timed (print . map snd . toList . distinct0Depths 5) ([6 ..] :: [Int])

d0d6IO :: IO ()
d0d6IO = mapM_Timed (print . map snd . toList . distinct0Depths 6) ([7 ..] :: [Int])

-- λ> (!!1).transpose$ d0d5s
-- [1,5,15,36,72,130,217,340,510,735,1027,1398,1861,2429,3119,3945,4925,6077,7420,8973,10759,12798,15114,17731,20674,23968,27642,31722,36238,41220,46699,52706,59276,66441,74237,82700,91867,101775,112465,123975,136347,149623,163846,179059,195309,212640,231100,250737,271600,293738,317204,342048,368324,396086,425389,456288,488842,523107,559143,597010,636769,678481,722211,768021,815977,866145,918592,973385,1030595,1090290,1152542,1217423,1285006,1355364,1428574,1504710,1583850,1666072,1751455,1840078,1932024,2027373,2126209,2228616,2334679,2444483,2558117,2675667,2797223,2922875,3052714,3186831,3325321,3468276]

-- λ> take 94$scanl(+)1.scanl(+)4.scanl(+)6.scanl(+)5$[-1,3,0]++cycle[0,4,-3,4,0,1]
-- [1,5,15,36,72,130,217,340,510,735,1027,1398,1861,2429,3119,3945,4925,6077,7420,8973,10759,12798,15114,17731,20674,23968,27642,31722,36238,41220,46699,52706,59276,66441,74237,82700,91867,101775,112465,123975,136347,149623,163846,179059,195309,212640,231100,250737,271600,293738,317204,342048,368324,396086,425389,456288,488842,523107,559143,597010,636769,678481,722211,768021,815977,866145,918592,973385,1030595,1090290,1152542,1217423,1285006,1355364,1428574,1504710,1583850,1666072,1751455,1840078,1932024,2027373,2126209,2228616,2334679,2444483,2558117,2675667,2797223,2922875,3052714,3186831,3325321,3468276]

-- λ> (!!2).transpose$ d0d5s
-- [2,7,16,28,47,72,104,144,193,253,324,406,502,613,738,879,1038,1215,1411,1627,1864,2124,2407,2713,3045,3404,3789,4202,4645,5118,5622,6158,6727,7331,7970,8644,9356,10107,10896,11725,12596,13509,14465,15465,16510,17602,18741,19927,21163,22450,23787,25176,26619,28116,29668,31276,32941,34665,36448,38290,40194,42161,44190,46283,48442,50667,52959,55319,57748,60248,62819,65461,68177,70968,73833,76774,79793,82890,86066,89322,92659,96079,99582,103168,106840,110599,114444,118377,122400,126513,130717,135013,139402,143886]

-- λ> take 94.scanl(+)2.scanl(+)5.scanl(+)4$[-1,4,-1]++cycle[1,1,1,2,0,0,3,1,-1,2,2,0]
-- [2,7,16,28,47,72,104,144,193,253,324,406,502,613,738,879,1038,1215,1411,1627,1864,2124,2407,2713,3045,3404,3789,4202,4645,5118,5622,6158,6727,7331,7970,8644,9356,10107,10896,11725,12596,13509,14465,15465,16510,17602,18741,19927,21163,22450,23787,25176,26619,28116,29668,31276,32941,34665,36448,38290,40194,42161,44190,46283,48442,50667,52959,55319,57748,60248,62819,65461,68177,70968,73833,76774,79793,82890,86066,89322,92659,96079,99582,103168,106840,110599,114444,118377,122400,126513,130717,135013,139402,143886]

-- λ> (!!3).transpose$ d0d5s
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94]
--
-- λ> ((!!3).transpose$ d0d5s) == [1..94]
-- True

d0d5s :: [[Int]]
d0d5s =
  [ [1]
  , [1,1,2,1]
  , [1,5,7,2]
  , [1,15,16,3]
  , [2,36,28,4]
  , [2,72,47,5]
  , [2,130,72,6]
  , [2,217,104,7]
  , [3,340,144,8]
  , [3,510,193,9]
  , [3,735,253,10]
  , [3,1027,324,11]
  , [4,1398,406,12]
  , [4,1861,502,13]
  , [4,2429,613,14]
  , [4,3119,738,15]
  , [5,3945,879,16]
  , [5,4925,1038,17]
  , [5,6077,1215,18]
  , [5,7420,1411,19]
  , [6,8973,1627,20]
  , [6,10759,1864,21]
  , [6,12798,2124,22]
  , [6,15114,2407,23]
  , [7,17731,2713,24]
  , [7,20674,3045,25]
  , [7,23968,3404,26]
  , [7,27642,3789,27]
  , [8,31722,4202,28]
  , [8,36238,4645,29]
  , [8,41220,5118,30]
  , [8,46699,5622,31]
  , [9,52706,6158,32]
  , [9,59276,6727,33]
  , [9,66441,7331,34]
  , [9,74237,7970,35]
  , [10,82700,8644,36]
  , [10,91867,9356,37]
  , [10,101775,10107,38]
  , [10,112465,10896,39]
  , [11,123975,11725,40]
  , [11,136347,12596,41]
  , [11,149623,13509,42]
  , [11,163846,14465,43]
  , [12,179059,15465,44]
  , [12,195309,16510,45]
  , [12,212640,17602,46]
  , [12,231100,18741,47]
  , [13,250737,19927,48]
  , [13,271600,21163,49]
  , [13,293738,22450,50]
  , [13,317204,23787,51]
  , [14,342048,25176,52]
  , [14,368324,26619,53]
  , [14,396086,28116,54]
  , [14,425389,29668,55]
  , [15,456288,31276,56]
  , [15,488842,32941,57]
  , [15,523107,34665,58]
  , [15,559143,36448,59]
  , [16,597010,38290,60]
  , [16,636769,40194,61]
  , [16,678481,42161,62]
  , [16,722211,44190,63]
  , [17,768021,46283,64]
  , [17,815977,48442,65]
  , [17,866145,50667,66]
  , [17,918592,52959,67]
  , [18,973385,55319,68]
  , [18,1030595,57748,69]
  , [18,1090290,60248,70]
  , [18,1152542,62819,71]
  , [19,1217423,65461,72]
  , [19,1285006,68177,73]
  , [19,1355364,70968,74]
  , [19,1428574,73833,75]
  , [20,1504710,76774,76]
  , [20,1583850,79793,77]
  , [20,1666072,82890,78]
  , [20,1751455,86066,79]
  , [21,1840078,89322,80]
  , [21,1932024,92659,81]
  , [21,2027373,96079,82]
  , [21,2126209,99582,83]
  , [22,2228616,103168,84]
  , [22,2334679,106840,85]
  , [22,2444483,110599,86]
  , [22,2558117,114444,87]
  , [23,2675667,118377,88]
  , [23,2797223,122400,89]
  , [23,2922875,126513,90]
  , [23,3052714,130717,91]
  , [24,3186831,135013,92]
  , [24,3325321,139402,93]
  , [24,3468276,143886,94]
  ]

-- It appears that for #(distinct elements) == 2:
--  (0) attains a maximum of (min 0 (m-2)) exactly once if (4 <= m)
--    And the depth of (1) is (0) for (0 <= m)
--  (0) attains a maximum of (m-3) 3 times for (6 <= m)
--    And the depth of (1) is (0)
--  (1) attains a maximum of (m-4) exactly once if (7 <= m)
--    And the depth of (0) is (1) for (7 <= m)


-- Here's the idea:
-- - We have a given length and number of distinct elements to allocate
-- - It's easy to find examples of low-depth patterns for single elements that are compatible, for example:
--   "0_0_0_0_0_0"
--    "1_1_1_1_1"

--   "0__0__0__0"
--    "1__1__1"
--     "2__2__2"
-- - Intuitively, high-depth single-element patterns should take up both:
--   * More series elements (since more elements are required for the larger depth)
--   * More "cycle space", i.e. they probabalistically take up more "prime real estate" for other values to increase their depth.

-- I'm kinda thinking of this as a game, where one player places their elements (must place first element at first available location),
--                                             and then the next player is trying to maximize their depth.

-- The first player can "win", from the data we have so far, but by how much?

-- Can we measure how much "stuff" there is to distribute among the elements to increase their individual depths?
--   Number of elements, sure, but what about the "prime real estate"? What makes a location "good" for increasing depth?

printDistinct0Depths :: Int -> IO ()
printDistinct0Depths n =
  mapM_Timed print . map (map snd . toList . distinct0Depths n) $ [(n + 1) ..]

-- | All the counts of depthds for 2 distinct elements of lengths [0..]
distinctDepths2 :: IO ()
distinctDepths2 =
  mapM_ (print . map snd . toList . distinctDepths 2) $ ([0 ..] :: [Int])

-- | Output from `distinctDepths2`
dd2 :: [[Int]]
dd2 =
  [ []
  , [2]
  , [1]
  , [3]
  , [5,2]
  , [6,8,1]
  , [7,18,5,1]
  , [8,35,16,3,1]
  , [9,58,46,10,3,1]
  , [10,92,111,29,9,3,1]
  , [11,139,251,72,25,9,3,1]
  , [12,199,541,171,63,24,9,3,1]
  , [13,266,1120,402,149,60,24,9,3,1]
  , [14,350,2227,922,342,144,59,24,9,3,1]
  , [15,449,4294,2083,778,335,141,59,24,9,3,1]
  , [16,571,8060,4664,1744,762,330,140,59,24,9,3,1]
  , [17,706,14831,10301,3874,1720,755,327,140,59,24,9,3,1]
  , [18,863,26770,22494,8528,3842,1708,750,326,140,59,24,9,3,1]
  , [19,1031,47495,48553,18630,8503,3830,1701,747,326,140,59,24,9,3,1]
  , [20,1218,83035,103492,40382,18664,8510,3818,1696,746,326,140,59,24,9,3,1]
  , [21,1422,143215,217918,86980,40664,18755,8500,3811,1693,746,326,140,59,24,9,3,1]
  , [22,1645,244136,453375,186317,87979,41038,18769,8488,3806,1692,746,326,140,59,24,9,3,1]
  , [23,1881,411753,932556,397385,189174,89188,41148,18759,8481,3803,1692,746,326,140,59,24,9,3,1]
  , [24,2139,687256,1898661,844207,404546,192659,89622,41164,18747,8476,3802,1692,746,326,140,59,24,9,3,1]
  , [25,2413,1135029,3829726,1787557,860925,413971,194054,89738,41154,18740,8473,3802,1692,746,326,140,59,24,9,3,1]
  , [26,2700,1854846,7659774,3772901,1824305,885288,417964,194506,89754,41142,18735,8472,3802,1692,746,326,140,59,24,9,3,1]
  , [27,3008,2999778,15200786,7939482,3851154,1885239,896033,419415,194624,89744,41135,18732,8472,3802,1692,746,326,140,59,24,9,3,1]
  , [28,3333,4801488,29951833,16657583,8102504,3999568,1912936,900210,419873,194640,89732,41130,18731,8472,3802,1692,746,326,140,59,24,9,3,1]
  , [29,3672,7608606,58629999,34848944,16995807,8456518,4068771,1924225,901678,419991,194630,89725,41127,18731,8472,3802,1692,746,326,140,59,24,9,3,1]
  ]

-- | All `distincts` whose resulting `IntPosMap` is of the given depth
ofDepth :: Int -> Int -> Int -> Forest Int
ofDepth n m d =
  stepFilters pureIntPosMap stepIntPosMap ((== d) . intPosMapDepth) $
  distincts n m


--  #(xs | length xs == m, n distwinct elements) =
--     We place the first distinct element: x0
--     Either x0 only occurs once,
--       in which case we are reduced to the case in which there are (m-1) element, (n-1) distinct elemenets,
--     Or x0 occurs multiple times,
--       in which case:
--         x0 must occur at least one more time
--         1 <= n
--         2 <= n + 1 <= m
--         a single instance of x0 can be placed anywhere in the series, and the remaining can be solved to give a valid series (supposing n, m satisfy the above)

--         depth $ x0 : not x0_1 == depth $ not x0_1
--         depth $ x0 : not x0_1 ++ [x0] == depth $ not x0_1
--         depth $ x0 : not x0_1 ++ [x0] == depth $ not x0_1
--         depth $ x0 : not x0_1 ++ [x0] ++ not x0_2
--           This should be depth of the (not x0) parts together, with an out-of-series gap.

-- Want:
--   Lists of (n) distinct elements




-- λ> :t (scanl(flip($))(pureIntPosMap 0)$stepIntPosMap<$>tail maxDiffSeries)
-- (scanl(flip($))(pureIntPosMap 0)$stepIntPosMap<$>tail maxDiffSeries)
--   :: [IntPosMap]

-- | `PosMap` specified to `Int` so that `IntMap` may be used.
data IntPosMap = IntPosMap
  { getIntPosMap :: IntMap PosCxt
  , intPosMapLen :: !Int
  } deriving (Eq, Ord, Show, Read, Typeable, Data, NFData, Generic)

instance Semigroup IntPosMap where
  IntPosMap mp1 len1 <> IntPosMap mp2 len2 = IntPosMap
    { getIntPosMap = IM.unionWith (<>) mp1 mp2
    , intPosMapLen = len1 + len2
    }

instance IsList IntPosMap where
  type Item IntPosMap = Int
  toList = toList . fromIntPosMap
  fromList =
    fromMaybe (error "IsList IntPosMap (fromList): empty list") . toIntPosMap


maxDepthRuns :: Int -> IO ()
maxDepthRuns =
  mapM_ (print . fmap (fmap runs)) . zip [1 ..] . maxTreeOn_ fst . maxDepthForN


-- | Tally all max depths and return
-- @[(length, tally (maxDepths at level of fseries))]@
tallyMaxDepths :: Int -> [(Int, IntMap Int)]
tallyMaxDepths =
  zipWith (\i mps -> (i, IM.unionsWith (+) mps)) [2 ..] . transpose .
  fmap
    (fmap (tally . fmap intPosMapDepth) .
     levels . stepTree stepIntPosMap pureIntPosMap) .
  fseries

tallyMaxDepthSums :: Int -> [(Int, [Int])]
tallyMaxDepthSums n = fmap (fmap (`div` n) . IM.elems) <$> tallyMaxDepths n


baseDepths :: Int -> Int -> [([Int], Int)]
baseDepths b = fmap (fmap intPosMapDepth) . scanIntPosMapL . (`toBase` b)

-- printAllBase2SeriesOfDepths :: IO ()
-- printAllBase2SeriesOfDepths = mapM_((>>putStrLn[]).uncurry(>>).bimap print(mapM_ print).fmap(baseDepths 2).join(,)) [0..]

-- permuting the labels of the values should have no effect on the resulting depth.

-- 505837410
-- 505837411
-- 505837412
-- 505837413
-- 505837414
-- 505837415
-- 505837416
-- 505837417
-- 505837418
-- 505837419
-- 505837420
-- 505837421
-- 505837422
-- ^C505837424
--
-- stack exec -- basic-exe  25900.09s user 5447.72s system 139% cpu 6:15:27.65 total
--
assertEqualUptoPerms2 :: Int -> Bool
assertEqualUptoPerms2 = liftM2(==)(intPosMapDepth.fromList)(intPosMapDepth.fromList.map(3-)).(`toBase` 2)

-- forall xs ix.
--   intPosMapDepth (xs | num vals == n) == intPosMapDepth (simplePerm n ix <$> xs)

-- It seems that the depth gives us some sort of "distance to all cycling"
-- or a measure of how non-cyclic our input is.

-- I'm not sure, but it may be the case that the depth ==
--   the minimum number of elements needed to be appended to the (right) end of the list to make it depth 0.


-- Ok, so it looks like it's valid, both from a common-sense standpoint (the maps shouldn't care what their keys are)
-- and from some basic testing.

-- Now, we need to figure out how to take advantage of this high amount of symmetry, since we can't "divide it out" directly.

-- The idea is to split the total up into a sum:
--   #(such that #(distict values) <= n) = sum [#(such that #(distinct values) == i) | i <- [1..n]]
-- Then we know that we can permute or replace values and keep the same result,
-- so we have something like #(total distinct values) `choose` #(distinct values) ways to choose values and #(distinct values)!
-- ways to permute those choices, so we're really factoring out a heavy amount of symmetry here.

-- The resulting problem has a fixed number of distinct values and counts results up to relabeling of values.

-- (On way to encode unique up to relabeling of values is to force every "smaller" value to appear before the appearance of a "bigger" value)
-- i.e. not0 all 0s, not0 1s, 0s <> 1s, not0 2s, 0s <> 1s <> 2s, ..

-- Now I'm not entirely sure how to count these, but it shouldn't be too hard to:
-- - generate them
-- - count the repetitions from the symmetries

-- Hopefully it'll result in a much faster computation.


-- | toBase is an inverse of fromBase:
-- λ> (\n->all ((`all` [0..n]) . liftM2(==)id.liftM2(.)fromBase(flip toBase))[2..n]) 1000
-- True
-- (5.28 secs, 1,521,236,536 bytes)


-- LEMMA:
--  It appears that the (last value - i) converges to a constant at: (6 + i)
--  I.e.:
--    f (y - i, y) = f (z - i, z) | 6 + i <= y, z

-- | tallyMaxDepthSums 2
ee' :: [(Int, [Int])]
ee' =
  [ (2,[1])
  , (3,[2])
  , (4,[4])
  , (5,[6,2])
  , (6,[7,8,1])
  , (7,[8,18,5,1])
  , (8,[9,35,16,3,1])
  , (9,[10,58,46,10,3,1])
  , (10,[11,92,111,29,9,3,1])
  , (11,[12,139,251,72,25,9,3,1])
  , (12,[13,199,541,171,63,24,9,3,1])
  , (13,[14,266,1120,402,149,60,24,9,3,1])
  , (14,[15,350,2227,922,342,144,59,24,9,3,1])
  , (15,[16,449,4294,2083,778,335,141,59,24,9,3,1])
  , (16,[17,571,8060,4664,1744,762,330,140,59,24,9,3,1])
  , (17,[18,706,14831,10301,3874,1720,755,327,140,59,24,9,3,1])
  , (18,[19,863,26770,22494,8528,3842,1708,750,326,140,59,24,9,3,1])
  , (19,[20,1031,47495,48553,18630,8503,3830,1701,747,326,140,59,24,9,3,1])
  , (20,[21,1218,83035,103492,40382,18664,8510,3818,1696,746,326,140,59,24,9,3,1])
  , (21,[22,1422,143215,217918,86980,40664,18755,8500,3811,1693,746,326,140,59,24,9,3,1])
  , (22,[23,1645,244136,453375,186317,87979,41038,18769,8488,3806,1692,746,326,140,59,24,9,3,1])
  , (23,[24,1881,411753,932556,397385,189174,89188,41148,18759,8481,3803,1692,746,326,140,59,24,9,3,1])
  ]

-- | tallyMaxDepthSums 3
ee3' :: [(Int, [Int])]
ee3' =
  [ (2,[1])
  , (3,[3])
  , (4,[9])
  , (5,[23,4])
  , (6,[51,28,2])
  , (7,[87,138,16,2])
  , (8,[117,516,84,10,2])
  , (9,[137,1564,422,52,10,2])
  , (10,[167,4308,1784,240,50,10,2])
  , (11,[195,11286,6944,976,220,50,10,2])
  , (12,[229,28338,25598,3732,872,218,50,10,2])
  , (13,[261,69064,89506,13920,3256,860,218,50,10,2])
  , (14,[299,164318,300254,50480,11744,3208,858,218,50,10,2])
  , (15,[335,382108,974792,179722,41456,11576,3196,858,218,50,10,2])
  ]

ee :: [[Int]]
ee = snd <$> ee'

ee3 :: [[Int]]
ee3 = snd <$> ee3'


ff = transpose ee
ff3 = transpose ee3


-- tt :: Int -> [(Int, [Int])]
tt n m = fmap (fmap (`div` (n-1)) . diff . diff) . transpose . take m $ fmap (`div` n) . IM.elems . snd <$> tallyMaxDepths n

checkAllsecondDiffsDivPredN n m = fmap (all ((== 0) . (`mod` (n-1))) . diff . diff) . transpose . take m $ fmap (`div` n) . IM.elems . snd <$> tallyMaxDepths n


middle1d :: Int -> Int -> Int
middle1d = fmap (intPosMapDepth . fromList) . middleOne

middle1ds :: Int -> Int -> (Int, Int)
middle1ds = fmap (unMaxes . intPosMapDepths . fromList) . middleOne


middle1dKeys :: Int -> Int -> IntMap Int
middle1dKeys = fmap (intPosMapKeyDepths . fromList) . middleOne


-- λ> (\n->mapM_ print$[toList.middle1dKeys x<$>[0..n]|x<-[0..n]]) 15
-- [[(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)],[(0,0),(1,0)]]
-- [[(0,0),(1,0)],[(0,0),(1,0)],[(0,1),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,7),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- [[(0,0),(1,0)],[(0,1),(1,0)],[(0,2),(1,0)],[(0,3),(1,0)],[(0,4),(1,0)],[(0,5),(1,0)],[(0,6),(1,0)],[(0,7),(1,0)],[(0,8),(1,0)],[(0,9),(1,0)],[(0,10),(1,0)],[(0,11),(1,0)],[(0,12),(1,0)],[(0,13),(1,0)],[(0,14),(1,0)],[(0,15),(1,0)]]
-- (0.06 secs, 21,000,040 bytes)


-- haskell/find-cycles » time stack exec -- basic-exe 3 12                                                                                                                                                                                                                            $
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- stack exec -- basic-exe 3 12  5.52s user 2.42s system 450% cpu 1.762 total
-- haskell/find-cycles » time stack exec -- basic-exe 4 10                                                                                                                                                                                                                            $
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- stack exec -- basic-exe 4 10  8.45s user 3.48s system 464% cpu 2.566 total
-- haskell/find-cycles » time stack exec -- basic-exe 5 8                                                                                                                                                                                                                             $
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- stack exec -- basic-ex

checkAllTallySumsDivisibleByN :: Int -> [(Int, Bool)]
checkAllTallySumsDivisibleByN n = fmap (all ((== 0) . (`mod` n)) . IM.elems) <$> tallyMaxDepths n


maxDepthForBools :: Pair [(Int, (Int, IntPosMap))] -- (length, depth, record-setter)
maxDepthForBools =
  zip [1 ..] .
  fmap (maximumBy (compare `on` fst) . fmap (first intPosMapDepth . join (,))) .
  blevels . stepMap (stepIntPosMap . fromEnum) (pureIntPosMap . fromEnum) <$>
  bools

-- | Print all of the maximum depths of `IntPosMap`'s with examples
ppMaxDepthForBools :: IO ()
ppMaxDepthForBools = loop maxDepthForBools
  where
    loop :: Pair [(Int, (Int, IntPosMap))] -> IO ()
    loop ~(Pair ((ix, (depthx, mpx)):xs, (iy, (depthy, mpy)):ys)) = do
      unless (ix == iy) . fail $ "ix /= iy: " ++ show (ix, iy)
      putStrLn $
        unlines
          [ "Current size: " ++ show ix
          , "False, .."
          , "Max depth: " ++ show depthx
          , "Record holder:"
          , ppIntPosMap' mpx
          , ""
          , "True, .."
          , "Max depth: " ++ show depthy
          , "Record holder:"
          , ppIntPosMap' mpy
          , ""
          ]
      loop $ Pair (xs, ys)


-- | The depth of an `IntPosMap`,
-- i.e. how many times we recurse in a particular `IntPosMap`
intPosMapDepth :: IntPosMap -> Int
intPosMapDepth = getMax . foldMap (Max . posCxtDepth) . getIntPosMap

intPosMapDepths :: IntPosMap -> Maxes Int
intPosMapDepths = foldMap (maxes . posCxtDepth) . getIntPosMap

intPosMapKeyDepths :: IntPosMap -> IntMap Int
intPosMapKeyDepths = fmap posCxtDepth . getIntPosMap


-- | The max depth tree of an `IntPosMap` for the given number of elements
maxDepthForN :: Int -> Tree (Int, [Int])
maxDepthForN =
  maxForestOn_ fst .
  fmap
    (fmap (swap . fmap intPosMapDepth) .
     stepTreeList stepIntPosMap pureIntPosMap) .
  fseries



-- | Show by converting maps to @{k1 => x1, k2 => x2, ..}@
ppIntPosMap :: IntPosMap -> String
ppIntPosMap = ppIntMap ppPosCxt . getIntPosMap

-- | Show using `drawForest` after converting to a `Forest`
ppIntPosMap' :: IntPosMap -> String
ppIntPosMap' = drawForest . fmap(fmap show) . intPosMapToForest

-- | `ppIntPosMap` for the element at @0@
ppIntPosMap0 :: IntPosMap -> String
ppIntPosMap0 = drawTree . fmap show . posCxtToTree 0 . (IM.! 0) . getIntPosMap

-- | `ppIntPosMap` for the element at @1@
ppIntPosMap1 :: IntPosMap -> String
ppIntPosMap1 = drawTree . fmap show . posCxtToTree 1 . (IM.! 1) . getIntPosMap

-- | Tree context for an `IntPosMap`.
--
-- @
-- (val, firstIx, lastIx, len, Maybe diff)
-- @
--
-- See `intPosMapToForest`
type Tcxt = (Int, Int, Int, Int, Maybe Int) -- (val, firstIx, lastIx, len, Maybe diff)

-- | Convert to a `Forest` for easier analysis
intPosMapToForest :: IntPosMap -> Forest Tcxt
intPosMapToForest = fmap (uncurry posCxtToTree) . IM.toAscList . getIntPosMap

-- | Pretty-print an `IntMap` using an explicit `show` method
ppIntMap :: (a -> String) -> IntMap a -> String
ppIntMap shw = ('[' :) . (++ "]") . intercalate ", " . fmap (uncurry ppSingleton) . IM.toAscList
  where
    ppSingleton k x = printf "%d => %s" k $ shw x


-- | A `IntPosMap` containing a single element
pureIntPosMap :: Int -> IntPosMap
pureIntPosMap x = IntPosMap
  { getIntPosMap = IM.singleton x $ purePosCxt 0
  , intPosMapLen = 1
  }

-- | Map over the top layer of keys in a `PosMap`
mapIntPosMap :: (Int -> Int) -> IntPosMap -> IntPosMap
mapIntPosMap f IntPosMap{..} = IntPosMap { getIntPosMap = IM.mapKeys f getIntPosMap, .. }

-- | Map over the top layer of keys monotonically in a `IntPosMap`
mapIntPosMapMonotonic :: (Int -> Int) -> IntPosMap -> IntPosMap
mapIntPosMapMonotonic f IntPosMap{..} = IntPosMap { getIntPosMap = IM.mapKeysMonotonic f getIntPosMap, .. }

-- | See `fromPosMap`
fromIntPosMap :: IntPosMap -> NonEmpty Int
fromIntPosMap (IntPosMap _ n)
  | n < 1 = error $ "fromIntPosMap: non-positive posMapLen: " ++ show n
fromIntPosMap (IntPosMap mp _) =
  fromList .
  fmap (\(~(Just x)) -> x) .
  foldl1 (zipWith (<|>)) . fmap (uncurry (fmap runMaybeT . posCxtToGapped)) $
  IM.toAscList mp

-- | Convert any non-empty `Foldable` to `Just` a `IntPosMap` by
-- applying `stepIntPosMap` to `pureIntPosMap` and then each
-- successive element, or any empty `Foldable` to `Nothing`.
toIntPosMap :: Foldable t => t Int -> Maybe IntPosMap
toIntPosMap  = foldl1Maybe (flip stepIntPosMap) pureIntPosMap

-- | `toIntPosMap`, but throw an error instead of returning `Nothing`
toIntPosMap' :: Foldable t => t Int -> IntPosMap
toIntPosMap' = fromMaybe (error "intPosMap': empty input") . toIntPosMap

-- | Add an element to a `IntPosMap` by incrementing its length
-- and `upsert`ing the element and position.
stepIntPosMap :: Int -> IntPosMap -> IntPosMap
stepIntPosMap x IntPosMap{..} = IntPosMap
  { getIntPosMap = intUpsert (purePosCxt intPosMapLen) (stepPosCxt intPosMapLen) x getIntPosMap
  , intPosMapLen = intPosMapLen + 1
  }


-- | The position context of an element, containing its first index (offset),
-- last index (for convenience), length (size), and `Either` `Right` a
-- recursive `IntPosMap` to store series more complex than a single-element cycle
-- or `Left` to indicate that it's a single-element cycle of that period (difference).
--
-- @maxIx :: Int@ using @maxIx@ instead of length allows us to quickly read off the index when adding values..
-- but if we're updating it anyway, we could use the old length as the new @maxIx@..
data PosCxt = PosCxt
  { firstIx :: !Int
  , lastIx :: !Int
  , posCxtLen :: !Int
  , ixSeries :: !(Either Int IntPosMap) -- ^ @`Left` x@ for all @x@'s
  } deriving (Eq, Ord, Read, Show, Typeable, Data, NFData, Generic)

-- | `undefined`
instance Semigroup PosCxt where
  PosCxt firstIx1 lastIx1 posCxtLen1 ixSeries1 <> PosCxt firstIx2 lastIx2 posCxtLen2 ixSeries2 =
    PosCxt
      { firstIx = min firstIx1 firstIx2
      , lastIx = max lastIx1 lastIx2
      , posCxtLen = posCxtLen1 + posCxtLen2
      , ixSeries = ixSeries'
      }
    where
      ixSeries' = undefined ixSeries1 ixSeries2

-- | Throws an error on empty lists
instance IsList PosCxt where
  type Item PosCxt = Int
  toList = posCxtIndices
  fromList =
    fromMaybe (error "IsList PosCxt (fromList): empty list") . toPosCxt

-- | The depth of an `PosCxt`,
-- i.e. how many times we recurse in a particular `PosCxt`
posCxtDepth :: PosCxt -> Int
posCxtDepth = either (const 0) ((1 +) . intPosMapDepth) . ixSeries

-- | Convert to a `Tree` for easier analysis
posCxtToTree :: Int -> PosCxt -> Tree Tcxt
posCxtToTree k PosCxt {..} =
  Node (k, firstIx, lastIx, posCxtLen, leftToMaybe ixSeries) $
  rightToList ixSeries >>= intPosMapToForest

-- | `Left` to `Just`
leftToMaybe :: Either a b -> Maybe a
leftToMaybe = Just `either` const Nothing

-- | `Right` to `[x]`
rightToList :: Either a b -> [b]
rightToList = liftM2 either const (flip (:)) []

-- [(a, .., [(a, .., ..


-- | Pretty-print a `PosCxt`
ppPosCxt :: PosCxt -> String
ppPosCxt PosCxt{..} = printf "PosCxt {[%d..%d] (%d) %s}" firstIx lastIx posCxtLen ppIxSeries
  where
    ppIxSeries = case ixSeries of
                   Left diff' -> show diff' ++ " ↻ "
                   ~(Right ixSeries') -> ppIntPosMap ixSeries'



-- | A `PosCxt` for a single repetition of an
-- element at the given index.
purePosCxt :: Int -> PosCxt
purePosCxt ix = PosCxt
  { firstIx = ix
  , lastIx = ix
  , posCxtLen = 1
  , ixSeries = Left 0
  }

-- | Add an element index to a `PosCxt`.
--
-- We increment the size and calculate the next decremented difference:
-- @(newIx - lastIx) - 1@.
--
-- If the `isSeries` is `Nothing` (i.e. representing a single-element
-- cycle), then we check whether it's continued (in which case we return
-- `Nothing`) or we need to make a new `IntPosMap` to represent the series.
stepPosCxt :: Int -> PosCxt -> PosCxt
stepPosCxt ix PosCxt {..} =
  PosCxt
    { lastIx = ix
    , posCxtLen = posCxtLen'
    , ixSeries =
        case ixSeries of
          Left 0 ->
            if ixDiff == 0 || posCxtLen == 1
              then Left ixDiff
              else Right $
                   IntPosMap
                     [(0, PosCxt {..}), (ixDiff, purePosCxt ix)]
                     posCxtLen'
          Left diff' ->
            if ixDiff == diff'
              then ixSeries
              else Right $
                   IntPosMap
                     [(diff', PosCxt {..}), (ixDiff, purePosCxt ix)]
                     posCxtLen'
          ~(Right ixSeries') -> Right $ stepIntPosMap ixDiff ixSeries'
    , ..
    }
  where
    posCxtLen' = posCxtLen + 1
    ixDiff = (ix - lastIx) - 1
-- ^ Just added the case for 0, should refactor as needed lated (e.g. could check that it's a singleton earlier)


-- | Convert any non-empty `Foldable` to a `PosCxt` by
-- applying `stepPosMap` to `purePosCxt` and then each
-- successive element.
toPosCxt :: Foldable t => t Int -> Maybe PosCxt
toPosCxt = foldl1Maybe (flip stepPosCxt) purePosCxt

-- | List, incl. offset, of whether or not the value of a
-- `PosCxt` is currently at that index.
--
-- For example, for the `PosCxt` of @1@ in @[0,1,1,0,0,1]@ `posCxtToBools`
-- would be:
--
-- @
--  [False, True, True, False, False, True]
-- @
--
posCxtToBools :: PosCxt -> [Bool]
posCxtToBools PosCxt {..} =
  replicate firstIx False ++ loop firstIx (posCxtIndices PosCxt {..})
  where
    loop :: Int -> [Int] -> [Bool]
    loop _ [] = []
    loop ix ~(x:xs) = loop1 ix x xs

    loop1 :: Int -> Int -> [Int] -> [Bool]
    loop1 ix x xs =
      if ix == x
        then True : loop (ix + 1) xs
        else False : loop1 (ix + 1) x xs

-- | Convert a `PosCxt` to a list of indices (positions) of its
-- associated element.
posCxtIndices :: PosCxt -> [Int]
posCxtIndices PosCxt {..} =
  case ixSeries of
    Left diff' -> take posCxtLen [firstIx, firstIx + diff' + 1 ..]
    ~(Right ixSeries') ->
      scanl ((+) . (1 +)) firstIx . toList $ fromIntPosMap ixSeries'

-- Gaps are `Nothing`
type Gapped = MaybeT

-- | If `True` then return the value, else `Nothing`.
maybeIf :: a -> Bool -> Maybe a
maybeIf _ False = Nothing
maybeIf x True  = Just x

-- | Convert a value and a its `PosCxt` to a list of
-- of `Just` the value where it occurs in the list
-- and `Nothing` (or the list ends) otherwise.
--
-- Every `Nothing` preceeds some `Just`. If there are
-- no more `Just`'s, the output ends.
posCxtToGapped :: a -> PosCxt -> Gapped [] a
posCxtToGapped x = MaybeT . fmap (maybeIf x) . posCxtToBools


return []
-- | `quickCheckAll`
runTests :: IO Bool
runTests = $quickCheckAll

