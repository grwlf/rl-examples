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
  mc_state :: pr -> g -> (s,g)
  mc_actions :: pr -> s -> Set a
  mc_transition :: (RandomGen g) => pr -> s -> a -> g -> (s,g)
  mc_reward :: pr -> s -> a -> s -> Maybe Reward
  mc_is_terminal :: pr -> s -> Bool

class (MC_Problem pr s a) => MC_Policy pr s a p where
  mcp_action :: (RandomGen g) => pr -> s -> p -> g -> (a,g)

-- | Builds an episode which is a list of transitions, terminal transition goes in head
episode :: (RandomGen g , MC_Policy pr s a p) => pr -> s -> p -> g -> ([(s,a,s)],g)
episode pr s p g =
  flip runRand g $ do
  snd <$> do
  flip execStateT (s,[]) $ do
  loop $ do
    let rnd = lift . lift . liftRand
    s <- gets fst
    a <- rnd $ mcp_action pr s p
    s' <- rnd $ mc_transition pr s a
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

data EvalState s = EvalState {
    -- _es_g :: Map s Rational
    _es_g :: Rational
  -- ^ Running return for the current episode
  , _es_v :: Map s Rational
  , _es_iter :: Int
  } deriving(Show)

makeLenses ''EvalState

initialEvalState v = EvalState mempty v 0

policy_eval :: forall p pr s a m g . (MC_Policy pr s a p, RandomGen g)
  => EvalOpts s a -> pr -> p -> StateVal s -> g -> (StateVal s, g)
policy_eval EvalOpts{..} pr p StateVal{..} g =
  flip runRand g $ do
  StateVal . view es_v <$> do
  flip execStateT (initialEvalState v_map) $ do
  loop $ do
    let rnd = lift . lift . liftRand
    ss <- rnd $ mc_state pr
    es <- rnd $ episode pr ss p
    es_g %= const 0
    forM_ es $ \(s,a,s') -> do
      case mc_reward pr s a s' of
        Just r -> es_g %= const r
        Nothing -> do
          r <- uses es_v (!s)
          es_g %= (+r)
      undefined
  -- undefined
  -- let sum l f = List.sum <$> forM (Set.toList l) f


