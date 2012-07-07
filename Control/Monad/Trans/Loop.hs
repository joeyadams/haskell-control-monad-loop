-- |
-- Module       : Control.Monad.Trans.Loop
-- Copyright    : (c) Joseph Adams 2012
-- License      : BSD3
-- Maintainer   : joeyadams3.14159@gmail.com
{-# LANGUAGE Rank2Types #-}
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

    -- * Lifting other operations
    liftLocalLoopT,
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
newtype LoopT c e m a = LoopT { runLoopT :: forall r. (c -> m r)
                                                   -> (e -> m r)
                                                   -> (a -> m r)
                                                   -> m r }

instance Functor (LoopT c e m) where
    fmap f m = LoopT $ \next fin cont -> runLoopT m next fin (cont . f)

instance Applicative (LoopT c e m) where
    pure a    = LoopT $ \_    _   cont -> cont a
    f1 <*> f2 = LoopT $ \next fin cont ->
                runLoopT f1 next fin $ \f ->
                runLoopT f2 next fin (cont . f)

instance Monad (LoopT c e m) where
    return a = LoopT $ \_    _   cont -> cont a
    m >>= k  = LoopT $ \next fin cont ->
               runLoopT m next fin $ \a ->
               runLoopT (k a) next fin cont

instance MonadTrans (LoopT c e) where
    lift m = LoopT $ \_ _ cont -> m >>= cont

instance MonadIO m => MonadIO (LoopT c e m) where
    liftIO = lift . liftIO


------------------------------------------------------------------------
-- continue and exit


-- | Skip the rest of the loop body and go to the next iteration.
continue :: LoopT () e m a
continue = continueWith ()

-- | Break out of the loop entirely.
exit :: LoopT c () m a
exit = exitWith ()

-- | Like 'continue', but return a value from the loop body.
continueWith :: c -> LoopT c e m a
continueWith c = LoopT $ \next _ _ -> next c

-- | Like 'exit', but return a value from the loop as a whole.
-- See the documentation of 'iterateLoopT' for an example.
exitWith :: e -> LoopT c e m a
exitWith e = LoopT $ \_ fin _ -> fin e


------------------------------------------------------------------------
-- Looping constructs


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
stepLoopT :: Monad m => LoopT c e m c -> (c -> m e) -> m e
stepLoopT body next = runLoopT body next return next

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
iterateLoopT :: Monad m => c -> (c -> LoopT c e m c) -> m e
iterateLoopT z body = loop z
  where loop c = stepLoopT (body c) loop


------------------------------------------------------------------------
-- Lifting other operations

-- | Lift a function like 'Control.Monad.Trans.Reader.local' or
-- 'Control.Exception.mask_'.
liftLocalLoopT :: Monad m => (forall a. m a -> m a) -> LoopT c e m b -> LoopT c e m b
liftLocalLoopT f cb = LoopT $ \next fin cont -> do
    m <- f $ runLoopT cb (return . next) (return . fin) (return . cont)
    m
