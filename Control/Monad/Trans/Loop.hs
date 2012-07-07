module Control.Monad.Trans.Loop (
    -- * The LoopT monad transformer
    LoopT(..),

    -- * continue and exit
    continue,
    exit,
    continueWith,
    exitWith,

    -- * Looping constructs
    while,
    foreach,
    stepLoopT,
    iterateLoopT,
) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | 'LoopT' is a monad transformer for the loop body.  It provides two
-- capabilities:
--
--  * 'continue' to the next iteration.
--
--  * 'exit' the whole loop.
newtype LoopT c r m a = LoopT { runLoopT :: (c -> m r) -> (a -> m r) -> m r }

instance Functor (LoopT c r m) where
    fmap f m = LoopT $ \next cont -> runLoopT m next (cont . f)

instance Applicative (LoopT c r m) where
    pure a    = LoopT $ \_    cont -> cont a
    f1 <*> f2 = LoopT $ \next cont ->
                runLoopT f1 next $ \f ->
                runLoopT f2 next (cont . f)

instance Monad (LoopT c r m) where
    return a = LoopT $ \_    cont -> cont a
    m >>= k  = LoopT $ \next cont ->
               runLoopT m next $ \a ->
               runLoopT (k a) next cont

instance MonadTrans (LoopT c r) where
    lift m = LoopT $ \_ cont -> m >>= cont

instance MonadIO m => MonadIO (LoopT c r m) where
    liftIO = lift . liftIO

-- | Skip the rest of the loop body and go to the next iteration.
continue :: LoopT () r m a
continue = continueWith ()

-- | Break out of the loop entirely.
exit :: Monad m => LoopT c () m a
exit = exitWith ()

-- | Like 'continue', but return a value from the loop body.
continueWith :: c -> LoopT c r m a
continueWith c = LoopT $ \next _ -> next c

-- | Like 'exit', but return a value from the loop as a whole.
-- See the documentation of 'iterateLoopT' for an example.
exitWith :: Monad m => r -> LoopT c r m a
exitWith r = LoopT $ \_ _ -> return r

------------------------------------------------------------------------

-- | Repeat the loop body while the predicate holds.  Like a @while@ loop in C,
-- the condition is tested first.
while :: Monad m => m Bool -> LoopT c () m c -> m ()
while cond body = loop
  where loop = do b <- cond
                  if b then stepLoopT body (\_ -> loop)
                       else return ()

-- | Call the loop body with each item in the list.
--
-- If you do not need to 'continue' or 'exit' the loop, consider using
-- 'Control.Monad.forM_' instead.
foreach :: Monad m => [a] -> (a -> LoopT c () m c) -> m ()
foreach list body = loop list
  where loop []     = return ()
        loop (x:xs) = stepLoopT (body x) (\_ -> loop xs)

-- | Call a loop body, passing it a continuation for the next iteration.
-- This can be used to construct custom looping constructs.  For example,
-- here is the definition of 'foreach':
--
-- >foreach list body = loop list
-- >  where loop []     = return ()
-- >        loop (x:xs) = stepLoopT (body x) (\_ -> loop xs)
stepLoopT :: LoopT c r m c -> (c -> m r) -> m r
stepLoopT body next = runLoopT body next next

-- | Call the loop body again and again, passing it the result of the previous
-- iteration each time around.  The only way to exit 'iterateLoopT' is to call
-- 'exit' or 'exitWith'.
--
-- Example:
--
-- >count :: Int -> IO Int
-- >count n = iterateLoopT 0 $ \i ->
-- >    if i < n
-- >        then do
-- >            lift $ print i
-- >            return $ i+1
-- >        else exitWith i
iterateLoopT :: c -> (c -> LoopT c r m c) -> m r
iterateLoopT z body = loop z
  where loop c = stepLoopT (body c) loop
