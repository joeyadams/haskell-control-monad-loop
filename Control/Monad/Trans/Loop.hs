-- |
-- Module       : Control.Monad.Trans.Loop
-- Copyright    : (c) Joseph Adams 2012
-- License      : BSD3
-- Maintainer   : joeyadams3.14159@gmail.com
--

{-# LANGUAGE Rank2Types #-}

-- Needed for the MonadBase instance
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Loop (
    -- * The LoopT monad transformer
    LoopT(..),
    stepLoopT,

    -- * continue and exit
    continue,
    exit,
    continueWith,
    exitWith,

    -- * Looping constructs
    foreach,
    while,
    doWhile,
    once,
    repeatLoopT,
    iterateLoopT,

    -- * Lifting other operations
    liftLocalLoopT,
) where

import Control.Applicative          (Applicative(pure, (<*>)))
import Control.Monad.Base           (MonadBase(liftBase), liftBaseDefault)
import Control.Monad.IO.Class       (MonadIO(liftIO))
import Control.Monad.Trans.Class    (MonadTrans(lift))

-- | 'LoopT' is a monad transformer for the loop body.  It provides two
-- capabilities:
--
--  * 'continue' to the next iteration.
--
--  * 'exit' the whole loop.
newtype LoopT c e m a = LoopT
    { runLoopT :: forall r.     -- This universal quantification forces the
                                -- LoopT computation to call one of the
                                -- following continuations.
                  (c -> m r)    -- continue
               -> (e -> m r)    -- exit
               -> (a -> m r)    -- return a value
               -> m r
    }

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

instance MonadBase b m => MonadBase b (LoopT c e m) where
    liftBase = liftBaseDefault

-- | Call a loop body, passing it a continuation for the next iteration.
-- This can be used to construct custom looping constructs.  For example,
-- here is the definition of 'foreach':
--
-- >foreach list body = loop list
-- >  where loop []     = return ()
-- >        loop (x:xs) = stepLoopT (body x) (\_ -> loop xs)
stepLoopT :: Monad m => LoopT c e m c -> (c -> m e) -> m e
stepLoopT body next = runLoopT body next return next

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


-- | Call the loop body with each item in the list.
--
-- If you do not need to 'continue' or 'exit' the loop, consider using
-- 'Control.Monad.forM_' instead.
foreach :: Monad m => [a] -> (a -> LoopT c () m c) -> m ()
foreach list body = loop list
  where loop []     = return ()
        loop (x:xs) = stepLoopT (body x) (\_ -> loop xs)

-- | Repeat the loop body while the predicate holds.  Like a @while@ loop in C,
-- the condition is tested first.
while :: Monad m => m Bool -> LoopT c () m c -> m ()
while cond body = loop
  where loop = do b <- cond
                  if b then stepLoopT body (\_ -> loop)
                       else return ()

-- | Like a @do while@ loop in C, where the condition is tested after
-- the loop body.
--
-- 'doWhile' returns the result of the last iteration.  This is possible
-- because, unlike 'foreach' and 'while', the loop body is guaranteed to be
-- executed at least once.
doWhile :: Monad m => LoopT a a m a -> m Bool -> m a
doWhile body cond = loop
  where loop = stepLoopT body $ \a -> do
            b <- cond
            if b then loop
                 else return a

-- | Execute the loop body once.  This is a convenient way to introduce early
-- exit support to a block of code.
--
-- 'continue' and 'exit' do the same thing inside of 'once'.
once :: Monad m => LoopT a a m a -> m a
once body = runLoopT body return return return

-- | Execute the loop body again and again.  The only way to exit 'repeatLoopT'
-- is to call 'exit' or 'exitWith'.
repeatLoopT :: Monad m => LoopT c e m a -> m e
repeatLoopT body = loop
  where loop = runLoopT body (\_ -> loop) return (\_ -> loop)

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
