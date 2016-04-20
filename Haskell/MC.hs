{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module MC where

import Control.Arrow ((&&&),(***))
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Random
import Control.Break
import Control.Lens (makeLenses, (%=), view, use, uses)
import Data.Ratio
import Data.List hiding (break)
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Data.Foldable
import Prelude hiding(break)
import System.Random
import Text.Printf

import Types as RL

class (Ord s) => MC_Problem pr s a | pr -> s , pr -> a where
  mc_state :: (RandomGen g) => pr -> g -> (s,g)
  mc_actions :: pr -> s -> Set a
  mc_transition :: (RandomGen g) => pr -> s -> a -> g -> (s,g)
  mc_reward :: pr -> s -> a -> s -> Reward
  mc_is_terminal :: pr -> s -> Bool

class (MC_Problem pr s a) => MC_Policy pr s a p where
  mcp_action :: (RandomGen g) => pr -> s -> p -> g -> (a,g)

-- | Builds an episode which is a list of transitions, terminal transition goes in head
episode :: (RandomGen g , MC_Policy pr s a p, MonadIO m) => pr -> s -> p -> g -> m ([(s,a,s)],g)
episode pr s p g = do
  flip runRandT g $ do
  snd <$> do
  flip execStateT (s,[]) $ do
  loop $ do
    let rnd = lift . lift . liftRandT
    s <- gets fst
    a <- rnd $ \g -> return $ mcp_action pr s p g
    s' <- rnd $ \g -> return $ mc_transition pr s a g
    liftIO $ putStr "."
    modify $ const s' *** ((s,a,s'):)
    when (mc_is_terminal pr s') $ do
      break ()

data EvalOpts s a = EvalOpts {
    eo_gamma :: Rational
  -- ^ Forgetness
  -- , eo_etha :: Rational
  -- ^ policy evaluation precision
  , eo_max_iter :: Int
  -- ^ policy evaluation iteration limit, [1..maxBound]
  -- , eo_floating_precision :: Double
  -- , eo_debug :: (StateVal s, GenericPolicy s a) -> IO ()
  } deriving(Show)

defaultOpts = EvalOpts {
    eo_gamma = 0.9
  -- , eo_etha = 0.1
  , eo_max_iter = 10^3
  -- , eo_floating_precision = 1/10^9
  -- , eo_debug = error "no debug specified"
  }

data Avg num = Avg {
    avg_curr :: num
  , avg_n :: num
  } deriving(Show)

initialAvg :: (Fractional num) => num -> Avg num
initialAvg num = Avg num 1

current :: (Fractional s) => Avg s -> s
current (Avg c n) = c

meld :: forall s . (Fractional s) => Avg s -> s -> Avg s
meld (Avg c n) s = Avg ((c*(n/(n+1))) + (s/(n + 1))) (n + 1)


data EvalState s = EvalState {
    -- _es_g :: Map s Rational
    _es_g :: Rational
  -- ^ Running return for the current episode
  , _es_v :: Map s (Avg Rational)
  , _es_iter :: Int
  , _es_visited :: Set s
  } deriving(Show)

makeLenses ''EvalState

initialEvalState :: (Ord s) => EvalState s
initialEvalState = EvalState 0 mempty 0 mempty


policy_eval :: forall p pr s a m g . (MC_Policy pr s a p, RandomGen g, MonadIO m, Show s, Show a)
  => EvalOpts s a -> pr -> p -> g -> m (StateVal s, g)
policy_eval EvalOpts{..} pr p g = do
  flip runRandT g $ do
  StateVal . Map.map current . view es_v <$> do
  flip execStateT initialEvalState $ do
  loop $ do
    i <- use es_iter
    es_iter %= (+1)
    when (i > eo_max_iter-1) $ do
      break ()

    let rnd = lift . lift . liftRandT
    ss <- rnd $ \g -> return $ mc_state pr g
    es <- rnd $ \g -> episode pr ss p g

    liftIO $ putStrLn $ "Episode " ++ show es
    es_g %= const 0
    es_visited %= const Set.empty
    forM_ es $ \(s,a,s') -> do
      fv <- uses es_visited (Set.member s')
      case fv of
        False -> do
          {- State s is already visited -}
          return ()
        True -> do
          {- First visit of state s for given episode -}
          es_visited %= (Set.insert s')
          es_g %= (+(mc_reward pr s a s'))
          g <- use es_g
          mv <- uses es_v (Map.lookup s)
          case mv of
            Just v -> do
              {- Melding new esitimates with all the previous -}
              es_v %= (Map.insert s (meld v g))
            Nothing -> do
              {- Act as V(s) is 0. Initialize it with current reward -}
              es_v %= (Map.insert s (initialAvg g))


