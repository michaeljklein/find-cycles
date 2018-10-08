import Data.IntPosMap
import Control.Monad

main :: IO ()
main = do
  passed <- runTests
  unless passed $ error "all tests did not pass"

