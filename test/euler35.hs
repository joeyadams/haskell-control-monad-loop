-- This solves Project Euler problem #35 [1].  The problem is to count the
-- number of circular primes [2] below one million.
--
--  [1]: http://projecteuler.net/problem=35
--  [2]: http://en.wikipedia.org/wiki/Circular_prime

{-# LANGUAGE BangPatterns #-}

import Control.Monad.Trans.Loop

import Control.Monad
import Control.Monad.Base
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.STRef

countCircularPrimes :: Int -> Int
countCircularPrimes e =
    runST $ do
        bmp   <- newArray (1, e) False :: ST s (STUArray s Int Bool)
        total <- newSTRef 0
        foreach (filter isPrime [2..e]) $ \i -> do
            -- If i is marked, we've already visited i and its rotations,
            -- so go on to the next prime.
            whenM (liftBase $ readArray bmp i)
                continue

            let rs = rotateDigits i

            -- Count the number of unique rotations.  We may end up marking a
            -- number with fewer digits, but that's okay because:
            --
            --  * We've already visited numbers with fewer digits.
            --
            --  * A circular prime will never contain the digit 0.
            --
            -- Thus, any counts affected by truncation will be discarded anyway.
            count <- liftBase $ newSTRef 0
            foreach rs $ \j -> do
                whenM (liftBase $ readArray bmp j)
                    exit
                liftBase $ writeArray bmp j True
                liftBase $ modifySTRef' count (+1)

            when (all isPrime rs) $ liftBase $
                total += count

        readSTRef total
  where
    sieve = mkSieve e
    isPrime p = sieve ! p

main :: IO ()
main = print $ countCircularPrimes 999999

------------------------------------------------------------------------
-- Helper functions

mkSieve :: Int -> UArray Int Bool
mkSieve e = runSTUArray $ do
    bmp <- newArray (2, e) True
    forM_ [2..e] $ \i ->
        whenM (readArray bmp i) $
            forM_ [i*2, i*3 .. e] $ \j ->
                writeArray bmp j False
    return bmp

-- | Return a list of every rotation of the decimal digits of a number.
rotateDigits :: Int -> [Int]
rotateDigits start
    | start < 1 = error "rotateDigits: n < 1"
    | otherwise = let (_, !r, !rs) = go 1
                   in rs [r]
  where
    go p | p' > start || p' < p = (p, start, id)
         | otherwise            = let (!factor, !r, !rs) = go p'
                                      (!n, !d) = r `divMod` 10
                                   in (factor, d * factor + n, rs . (r :))
      where
        p' = p*10

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' ref f = do
    x <- readSTRef ref
    writeSTRef ref $! f x

(+=) :: STRef s Int -> STRef s Int -> ST s ()
(+=) total n = readSTRef n >>= modifySTRef' total . (+)

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= \b -> if b then m else return ()
