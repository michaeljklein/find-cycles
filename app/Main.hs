module Main where

-- import Lib
-- import Data.Semigroup.Free
import Data.Semigroup.Free
import Data.IntPosMap
import System.Environment

-- main :: IO ()
-- main = do
  -- [n, m] <- fmap read <$> getArgs
  -- [n] <- fmap read <$> getArgs
  -- printDistinct0Depths n
  -- maxDepthRuns n
  -- mapM_ print $ tallyMaxDepthSums n -- :: Int -> [(Int, [Int])]
  -- mapM_ print $ tallyMaxDepths n
  -- mapM_ print $ tt n m



-- main = asserts $ assertEqualUptoPerms2 <$> [0..] :: IO ()

main = d0d6IO
-- main = d0d5IO
-- main = distinctDepths2
-- main = ppMaxDepthForBools
-- main = runWeighFS
-- main = runBenchFS
-- main = someFunc
