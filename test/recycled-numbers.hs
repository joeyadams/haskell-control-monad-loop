-- This solves Google Code Jam 2012 Qualification Problem C "Recycled Numbers" [1].
-- The problem is: given a range of numbers with the same number of digits,
-- count how many pairs of them are the same modulo rotation of digits.
--
--  [1]: http://code.google.com/codejam/contest/1460488/dashboard#s=p2
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.Trans.Loop

import Control.Applicative          ((<$>))
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Data.Array.ST
import Data.STRef

recycledNumbers :: (Int, Int) -> Int
recycledNumbers (lb, ub)
    | not (1 <= lb && lb <= ub && factor == rotateFactor ub)
    = error "recycledNumbers: invalid bounds"
    | otherwise = runST $ do
        bmp   <- newArray (lb, ub) False :: ST s (STUArray s Int Bool)
        total <- newSTRef 0
        forM_ [lb..ub] $ \i -> do
            count <- newSTRef 0
            foreach (iterate rotate i) $ \j -> do
                when (not $ j >= i && j <= ub)
                    continue
                whenM (lift $ readArray bmp j)
                    exit
                lift $ writeArray bmp j True
                lift $ modifySTRef' count (+1)
            readSTRef count >>= modifySTRef' total . (+) . numPairs
        readSTRef total
  where
    factor = rotateFactor lb

    rotate x = let (n, d) = x `divMod` 10
                in d*factor + n

    numPairs n = (n-1) * n `div` 2

main :: IO ()
main = do
    t <- readLn
    forM_ [1..t] $ \(x :: Int) -> do
        [a, b] <- map read . words <$> getLine
        let y = recycledNumbers (a, b)
        putStrLn $ "Case #" ++ show x ++ ": " ++ show y

------------------------------------------------------------------------
-- Helper functions

-- | Return the power of 10 corresponding to the most significant digit in the
-- number.
rotateFactor :: Int -> Int
rotateFactor n | n < 1     = error "rotateFactor: n < 1"
               | otherwise = loop 1
  where
    loop p | p' > n    = p
           | p' < p    = p     -- in case of overflow
           | otherwise = loop p'
      where p' = p * 10

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' ref f = do
    x <- readSTRef ref
    writeSTRef ref $! f x

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= \b -> if b then m else return ()
