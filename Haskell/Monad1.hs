{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- NOTE: See FIXME below
{-# LANGUAGE ImpredicativeTypes #-}

module Monad where

import Imports


class (Monad m) => MonadRnd m where
  getRndR :: (Random a) => (a,a) -> m a
  getGen :: forall g . (RandomGen g) => m g
  putGen :: forall g . (RandomGen g) => g -> m ()
  roll :: (forall g . (RandomGen g) => g -> (a,g)) -> m a


newtype RndT g m a = RndT { unRndT :: StateT g m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

runRndT :: RndT g m a -> g -> m (a,g)
runRndT r g = runStateT (unRndT r) g

-- | Sample a random value from a weighted list.  The total weight of all
-- elements must not be 0.
fromList :: (MonadRnd m) => [(a,Rational)] -> m a
fromList [] = error "MonadRnd.fromList called with empty list"
fromList [(x,_)] = return x
fromList xs = do
  -- TODO: Do we want to be able to use floats as weights?
  -- TODO: Better error message if weights sum to 0.
  let s = (fromRational (sum (map snd xs))) :: Double -- total weight
      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs       -- cumulative weight
  p <- liftM toRational $ getRndR (0.0,s)
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs

-- | Sample a value from a uniform distribution of a list of elements.
uniform :: (MonadRnd m) => [a] -> m a
uniform = Monad.fromList . fmap (flip (,) 1)

instance (Monad m, RandomGen g) => MonadRnd (RndT g m) where
  getGen = RndT $ do
    g <- get
    return g

  putGen g = undefined

  getRndR (x,y) = RndT . state $ randomR (x,y)
  roll f = RndT $ do
    g <- get
    (a,g') <- pure (f g)
    put g'
    return a

  -- rollM f = do
  --   g <- RndT get
  --   (a,g') <- f g
  --   RndT (put g')
  --   return a

-- liftRollM f = do
--   g <- lift $ RndT get
--   (a,g') <- f g
--   lift $ RndT (put g')
--   return a

instance (MonadRnd m) => MonadRnd (StateT s m) where
  getRndR = lift . getRndR
  roll = lift . roll
  -- rollM f = StateT $ \s -> do
  --   rollM $ \g -> do
  --     ((a,g'),s') <- runStateT (f g) s
  --     return ((a,s'), g')

instance (MonadRnd m) => MonadRnd (Break r m) where
  getRndR = lift . getRndR
  -- FIXME: Unfoirtunately, Control.Break hides constructor of Break, so we are
  -- to use lift interface with ImpredicativeTypes pragma
  roll = lift . roll
  -- rollM = undefined

