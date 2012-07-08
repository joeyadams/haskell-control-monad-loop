{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Loop

main :: IO ()
main = do
    foreach [1..10] $ \(i :: Int) -> do
        foreach [1..10] $ \(j :: Int) -> do
            when (j > i) $
                lift continue
            when (i == 2 && j == 2) $
                exit
            when (i == 9 && j == 9) $
                lift exit
            liftBase $ print (i, j)
        liftBase $ putStrLn "Inner loop finished"
    putStrLn "Outer loop finished"
