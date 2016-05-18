{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
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
module MC (
    module MC
  , module MC.Types
  ) where

import Imports
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding(break)

import Types as RL
import DP (DP_Problem(..), DP_Policy(..))
import qualified DP as DP

import MC.Types

{-
    _    _
   / \  | | __ _
  / _ \ | |/ _` |
 / ___ \| | (_| |
/_/   \_\_|\__, |
           |___/
-}


-- | Builds an episode which is a list of transitions, terminal transition is near head
episode :: (RandomGen g , MC_Policy num pr s a p, MonadIO m, Show s, Show a) => pr num -> s -> p -> g -> m (Episode s a,g)
episode pr s p g = do
  flip runRandT g $ do
  Episode . snd <$> do
  flip execStateT (s,[]) $ do
  loop $ do
    let rnd m = lift $ lift $ liftRandT $ (\g -> return $ m g)
    s <- gets fst
    case (mc_is_terminal pr s) of
      True -> do
        break ()
      False -> do
        a <- rnd $ mc_action pr s p
        s' <- rnd $ mc_transition pr s a
        modify $ const s' *** ((s,a,s'):)

-- Backtrack rewards, first visit counts
backtrack_fv :: (MC_Problem num pr s a) => pr num -> Episode s a -> Map s num
backtrack_fv pr ep =
  view _1 $ flip execState (Map.empty, 0) $ do
    forM_ (episode_backward ep) $ \ (s,a,s') -> do
      r <- pure $ mc_reward pr s a s'
      _2 %= (+r)
      g <- use _2
      _1 %= (Map.insert s g)



data EvalState num s = EvalState {
    _es_v :: Map s (Avg num)
  , _es_iter :: Integer
  } deriving(Show)

makeLenses ''EvalState

initialEvalState :: (Ord s, Fractional num) => EvalState num s
initialEvalState = EvalState mempty 0

-- | DIfference between state value estimates
-- FIXME: handle missing states case
diffVal :: (Ord s, Fractional num) => StateVal num s -> (Map s (Avg num)) -> num
diffVal (v_map -> tgt) src = sum $ Map.intersectionWith (\a b -> abs $ a - (current b)) tgt src


-- Monte carlo policy evaluation, Figure 5.1. pg 109
policy_eval :: (MC_Policy_Show num pr s a p, RandomGen g, MonadIO m, Real num)
  => EvalOpts num s a -> pr num -> p -> g -> m (StateVal num s, g)
policy_eval EvalOpts{..} pr p = do
  runRandT $ do
  StateVal . Map.map current . view es_v <$> do
  flip execStateT initialEvalState $ do
  loop $ do
    i <- use es_iter
    es_iter %= (+1)
    when (i > eo_max_iter-1) $ do
      break ()

    let rnd = lift . lift . liftRandT
    ss <- rnd $ return . mc_state pr
    es <- rnd $ episode pr ss p
    gs <- pure $ backtrack_fv pr es

    forM_ (Map.toList gs) $ \(s,g) -> do
      v <- fromMaybe initialAvg <$> uses es_v (Map.lookup s)
      es_v %= Map.insert s (meld v g)

    case eo_learnMonitor of
      Nothing -> return ()
      Just Monitor{..} -> do
        v <- use es_v
        push mon_data (fromInteger i) (diffVal mon_target v)

