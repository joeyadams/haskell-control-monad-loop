module Control.Monad.Trans.Loop where

import Control.Monad.Trans.Class

-- | 'LoopT' is a monad transformer for the loop body, 
newtype LoopT c r m a = LoopT { runLoopT :: (c -> m r) -> (a -> m r) -> m r }

instance Monad (LoopT c r m) where
    m >>= k = LoopT $ \next cont ->
              runLoopT m next $ \a ->
              runLoopT (k a) next cont

    return a = LoopT $ \_ cont -> cont a

instance MonadTrans (LoopT c r) where
    lift m = LoopT $ \_ cont -> m >>= cont

-- | Skip the rest of the loop body and go to the next iteration.
continueLoop :: c -> LoopT c r m a
continueLoop c = LoopT $ \next _ -> next c

-- | Break out of the loop entirely.
breakLoop :: Monad m => r -> LoopT c r m a
breakLoop r = LoopT $ \_ _ -> return r

------------------------------------------------------------------------

-- | Call the loop body, passing it a continuation for the next iteration.
-- This can be used to construct custom looping constructs; see the source of
-- 'foreach' for a simple example.
stepLoopT :: LoopT c r m c -> (c -> m r) -> m r
stepLoopT body next = runLoopT body next next

iterateLoopT :: c -> (c -> LoopT c r m c) -> m r
iterateLoopT z body = loop z
  where loop c = stepLoopT (body c) loop

foreach :: Monad m => [a] -> (a -> LoopT c () m c) -> m ()
foreach list body = loop list
  where loop []     = return ()
        loop (x:xs) = stepLoopT (body x) (\_ -> loop xs)

while :: Monad m => m Bool -> LoopT c () m c -> m ()
while cond body = loop
  where loop = do b <- cond
                  if b then stepLoopT body (\_ -> loop)
                       else return ()
