
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

import MC.Types as MC

-- | Builds an episode which is a list of transitions, terminal transition goes
-- to the head of Episode list
episode :: (MC_Policy num pr s a p,
            MC_Problem_Show num pr s a,
            MonadIO m, MonadRnd g m) =>
  Opts num ext -> pr num -> s -> a -> p -> m (Maybe (Episode s a))
episode Opts{..} pr s a p = do
  e <- do
    flip execStateT (s, Just a, [], False) $ do
    loop $ do
      s <- use _1
      a <- use _2 >>=
            \case
              Just a -> pure a
              Nothing -> roll $ mc_action pr s p
      (s',fin) <- roll $ mc_transition pr s a
      _1 %= const s'
      _2 %= const Nothing
      _3 %= ((s,a,s'):)
      _4 %= const fin

      len <- uses _3 (toInteger . length)
      when (fin || len > o_maxEpisodeLen) $ do
        break ()

  if view _4 e then
    return $ Just (Episode $ view _3 e)
  else
    return Nothing

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


data ES_Ext num s a = ES_Ext {
  eo_debug :: ES_State num s a -> IO ()
}

type ES_Opts num s a = Opts num (ES_Ext num s a)

defaultOpts :: (Fractional num) => ES_Opts num s a
defaultOpts = MC.defaultOpts ES_Ext{
  eo_debug = const (return ())
}

-- | Figure 5.4 pg 116
policy_iteraton :: (MC_Policy num pr s a (GenericPolicy s a),
                    MC_Problem_Show num pr s a,
                    RandomGen g, MonadIO m,
                    Fractional num,
                    Ord a, Ord num, Real num)
  => ES_Opts num s a
  -> pr num
  -> (Q num s a, GenericPolicy s a)
  -> g
  -> m ((Q num s a, GenericPolicy s a), g)

policy_iteraton o@Opts{..} pr (q,p) = do
  runRndT $ do
  (view ess_q &&& view ess_p) <$> do
  flip execStateT (ES_State q p 0) $ do
  loop $ do

    {- Debug -}
    get >>= liftIO . (eo_debug o_ext)

    i <- use ess_iter
    ess_iter %= (+1)
    when (i > o_max_iter-1) $ do
      break ()

    {- Episode generation -}
    s <- roll $ mc_state_nonterm pr
    a <- RL.uniform $ Set.toList (mc_actions pr s)
    p <- use ess_p
    me <- episode o pr s a p

    case me of

      Nothing -> do
        traceM "Bad episode"

      Just e -> do
        gs <- backtrack_fv pr <$> pure e

        {- Policy evaluation -}
        forM_ (Map.toList gs) $ \(s,as) -> do
          forM_ (Map.toList as) $ \(a,g) -> do
            (ess_q . q_map) %= Map.unionWith (Map.unionWith combineAvg) (
                                 Map.singleton s (Map.singleton a (singletonAvg g)))


        {- Policy improvement -}
        forM_ (Map.toList gs) $ \(s,as) -> do
          q_s <- uses (ess_q . q_map) (Map.toList . (!s))
          (ess_p . p_map) %=
            let
              (abest, nmax) = maximumBy (compare `on` (current . snd)) q_s
            in
            Map.insert s (Set.singleton (abest, 1%1))


