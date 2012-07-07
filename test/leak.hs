-- Make sure basic loops don't leak memory

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Loop
import Control.Monad.Trans.State.Strict
import Data.Int (Int64)

count :: Int64 -> IO Int64
count n = iterateLoopT 0 $ \i ->
    if i < n
        then return $! i+1
        else exitWith i

sumLoop :: [Int64] -> Int64
sumLoop list =
    flip execState 0 $ foreach list $ \i -> do
        when (i == 10000000) exit
        lift $ modify' (+i)
  where
    modify' f = do
        x <- get
        put $! f x

main :: IO ()
main = do
    count 100000000 >>= print
    print $ sumLoop [1..10000000] + 10000000
