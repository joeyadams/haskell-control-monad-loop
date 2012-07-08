{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (log)

import Control.Monad.Trans.Loop

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Writer

test1 :: IO ()
test1 =
    foreach [1..4] $ \(i :: Int) -> do
        let log msg = liftIO $ putStrLn $ "test1: " ++ show i ++ ": " ++ msg

            logMaskingState = do
                b <- lift getMaskingState
                log $ "getMaskingState: " ++ show b

        logMaskingState

        liftLocalLoopT mask_ $ do
            logMaskingState
            when (i == 3) $ do
                log "continue"
                continue
            logMaskingState

-- This test is interesting because we're using mtl's 'local', which in this
-- context walks up the WriterT too.
test2 :: IO ()
test2 =
    mapM_ putStrLn $
    flip runReader (0 :: Int) $
    execWriterT $
    foreach [1..4] $ \(i :: Int) -> do
        let log msg = lift $ tell ["test2: " ++ show i ++ ": " ++ msg]

            logAsk = do
                n <- lift ask
                log $ "ask: " ++ show n

        logAsk

        liftLocalLoopT (local (+1)) $ do
            logAsk
            when (i == 3) $ do
                log "continue"
                continue
            logAsk
