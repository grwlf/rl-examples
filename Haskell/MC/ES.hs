
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
module MC.ES where

import Imports
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding(break)

import Types as RL
import Monad as RL
import DP (DP_Problem(..), DP_Policy(..))
import qualified DP as DP

import MC.Types

-- | Builds an episode which is a list of transitions, terminal transition goes
-- to the head of Episode list
episode :: (MC_Policy num pr s a p, MonadIO m, MonadRnd g m, Show s, Show a) =>
  pr num -> s -> a -> p -> m (Episode s a)
episode pr s a p = do
  Episode . view _3 <$> do
  flip execStateT (s, Just a, []) $ do
  loop $ do
    s <- use _1
    case (mc_is_terminal pr s) of
      True -> do
        break ()
      False -> do
        a <- use _2 >>= \case
          Just a -> pure a
          Nothing -> roll $ mc_action pr s p
        s' <- roll $ mc_transition pr s a
        _1 %= const s'
        _2 %= const Nothing
        _3 %= ((s,a,s'):)

-- Backtrack rewards, first visit counts
backtrack_fv :: (MC_Problem num pr s a, Ord a) => pr num -> Episode s a -> Map s (Map a num)
backtrack_fv pr ep =
  view _1 $ flip execState (Map.empty, 0) $ do
    forM_ (episode_backward ep) $ \ (s,a,s') -> do
      r <- pure $ mc_reward pr s a s'
      _2 %= (+r)
      g <- use _2
      _1 %= Map.unionWith (Map.unionWith (\a b -> a)) (
              Map.singleton s (Map.singleton a g))


data ES_State num s a = ES_State {
    _ess_q :: Q num s a
  , _ess_p :: GenericPolicy s a
  , _ess_iter :: Integer
  } deriving(Show)

makeLenses ''ES_State


-- | Figure 5.4 pg 116
policy_iteraton :: (MC_Problem_Show num pr s a, RandomGen g, MonadIO m, Fractional num, MC_Policy num pr s a (GenericPolicy s a), Ord a, Ord num)
  => EvalOpts num s a -> pr num -> (Q num s a, GenericPolicy s a) -> g -> m ((Q num s a, GenericPolicy s a), g)
policy_iteraton EvalOpts{..} pr (q,p) = do
  runRndT $ do
  (view ess_q &&& view ess_p) <$> do
  flip execStateT (ES_State q p 0) $ do
  loop $ do
    s <- roll $ mc_state pr
    a <- RL.uniform $ Set.toList (mc_actions pr s)
    p <- use ess_p
    gs <- backtrack_fv pr <$> episode pr s a p

    forM_ (Map.toList gs) $ \(s,as) -> do
      forM_ (Map.toList as) $ \(a,g) -> do
        (ess_q . q_map) %= Map.unionWith (Map.unionWith combineAvg) (
                             Map.singleton s (Map.singleton a (singletonAvg g)))

    forM_ (Map.toList gs) $ \(s,as) -> do
      q_s <- uses (ess_q . q_map) (Map.toList . (!s))
      (ess_p . p_map) %=
        let
          (abest, nmax) = maximumBy (compare `on` (current . snd)) q_s
        in
        Map.insert s (Set.singleton (abest, 1%1))

    break ()

