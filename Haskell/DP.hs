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
module DP where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Break
import Control.Lens (makeLenses, (%=), view, use, uses)
import Data.Ratio
import Data.List hiding (break)
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding(break)
import Data.Foldable
import Text.Printf
import Debug.Trace

debug :: (MonadIO m) => String -> m ()
debug = liftIO . putStrLn

type Point = (Int,Int)

type Probability = Rational
type Reward = Double

data StateVal s = StateVal {
    v_map :: Map s Double
  } deriving(Show)

class (Ord s) => RLProblem p s a | p -> s , p -> a where
  rl_states :: p -> Set s
  rl_transitions :: p -> s -> a -> Set (Probability, (Reward, s))

rl_prob_invariant :: forall p s a . (RLProblem p s a) => p -> s -> a -> Bool
rl_prob_invariant p s a = 1%1 == List.sum (map fst (Set.toList $ rl_transitions p s a))

class (RLProblem pr s a) => RLPolicy pp pr s a where
  rlp_action :: pp -> pr -> s -> Set (Probability, a)

zero_sate_values :: forall p s a m . (RLProblem p s a)
  => p -> StateVal s
zero_sate_values p =  StateVal $ Map.fromList $ map (\s -> (s,0.0)) (Set.toList $ rl_states p)

data EvalOpts = EvalOpts {
    eo_gamma :: Double
  , eo_etha :: Double
  , eo_max_iter :: Int
  } deriving(Show)

defaultOpts = EvalOpts {
    eo_gamma = 0.9
  , eo_etha = 0.1
  , eo_max_iter = 10^3
  }

data EvalState s = EvalState {
    _es_delta :: Double
  , _es_v :: Map s Double
  , _es_v' :: Map s Double
  , _es_iter :: Int
  } deriving(Show)

makeLenses ''EvalState

initEvalState StateVal{..} = EvalState 0.0 v_map v_map 0

-- | Iterative policy evaluation algorithm
-- Figure 4.1, pg.86.
policy_eval :: forall pol p s a m . (RLPolicy pol p s a, MonadIO m)
  => p -> pol -> EvalOpts -> StateVal s -> m (StateVal s)
policy_eval p pol EvalOpts{..} v = do
  let sum l f = List.sum <$> forM (Set.toList l) f

  StateVal . view es_v <$> do
    flip execStateT (initEvalState v) $ loop $ do

      i <- use es_iter
      when (i > eo_max_iter-1) $ do
        break ()

      es_delta %= const 0.0

      forM_ (rl_states p) $ \s -> do
        v_s <- uses es_v (!s)
        v's <- do
          sum (rlp_action pol p s) $ \(fromRational -> pa, a) -> do
            (pa*) <$> do
              sum (rl_transitions p s a) $ \(fromRational -> p, (r, s')) -> do
                v_s' <- uses es_v (!s')
                pure $ p * (r + eo_gamma * (v_s'))

        es_v' %= (Map.insert s v's)
        es_delta %= (`max`(abs (v's - v_s)))

      d <- use es_delta
      when (d < eo_etha) $ do
        break ()

      v' <- use es_v'
      es_v %= const v'

      es_iter %= (+1)


data GenericPolicy s a = GenericPolicy {
  gp_actions :: Map s (Set (Probability,a))
  } deriving(Eq,Ord, Show)

instance (RLProblem p s a) => RLPolicy (GenericPolicy s a) p s a where
  rlp_action GenericPolicy{..} _ s = gp_actions ! s

-- data PIState = PIState {
--   pi_actions :: Map s (Set (Probability,a))
--   }

-- FIXME:check
policy_improve :: forall pol p s a m . (RLPolicy pol p s a, MonadIO m, Ord a)
  => p -> pol -> EvalOpts -> StateVal s -> m (GenericPolicy s a)
policy_improve p pol EvalOpts{..} StateVal{..} = do
  let sum l f = List.sum <$> forM (Set.toList l) f

  GenericPolicy <$> do
    flip execStateT Map.empty $ do

      forM_ (rl_states p) $ \s -> do
        (maxv, maxa) <- do
          foldlM (\(val,maxa) (fromRational -> pa, a) -> do
                    pi_s <- do
                      sum (rl_transitions p s a) $ \(fromRational -> p, (r, s')) -> do
                        v_s' <- pure (v_map ! s')
                        pure $ p * (r + eo_gamma * (v_s'))
                    return $
                      if Set.null maxa then
                        (pi_s, Set.singleton a)
                      else
                        case pi_s `compare` val of
                          GT -> (pi_s, Set.singleton a)
                          LT -> (val,maxa)
                          EQ -> (pi_s, Set.insert a maxa)
                 ) (0.0 ,Set.empty) (rlp_action pol p s)

        let nmax = toInteger (Set.size maxa)
        modify $ Map.insert s (Set.map (\a -> (1%nmax,a)) maxa)


