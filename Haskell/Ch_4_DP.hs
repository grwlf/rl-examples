{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Ch_4_DP where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Break
import Data.Ratio
import Data.List hiding (break)
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding(break)
import Text.Printf

type Point = (Int,Int)

type Probability = Rational
type Reward = Double

-- data Board = Board {
--     b_states :: Set Point
--   , b_map :: Map Point Value
--   , b_loc :: Point
--   }

data StateVal s = StateVal {
    v_map :: Map s Double
  }

class (Ord s) => RLProblem p s a | p -> s , p -> a where
  rl_states :: p -> Set s
  rl_actions :: p -> s -> Set a
  rl_transitions :: p -> s -> a -> Set (Probability, (Reward, s))


rl_prob_invariant :: forall p s a . (RLProblem p s a) => p -> s -> a -> Bool
rl_prob_invariant p s a = 1%1 == List.sum (map fst (Set.toList $ rl_transitions p s a))


data Policy s a = Policy {
    pol_map :: Map s a
  }

data EvalOpts = EvalOpts {
    eo_gamma :: Double
  }

policy_eval :: forall p s a m . (RLProblem p s a, MonadIO m)
  => p -> Policy s a -> EvalOpts -> StateVal s -> m (StateVal s)
policy_eval p Policy{..} EvalOpts{..} StateVal{..} = do
  let v = v_map
  let sum l f = List.sum <$> forM (Set.toList l) f
  let get_delta = gets fst
  let put_delta d = modify (\(_,v) -> (d, v))
  let get_v s = (! s) <$> gets snd
  let put_v s v_s = modify (\(d,v) -> (d, Map.insert s v_s v))

  StateVal . snd <$> do
    flip execStateT (0,v) $ loop $ do

      d <- get_delta
      when (d < eo_gamma) $ do
        break ()

      forM_ (rl_states p) $ \s -> do
        v_s <- get_v s
        v_s' <- do
          sum (rl_actions p s) $ \a -> do

            sum (rl_transitions p s a) $ \(p, (r, s')) -> do
              pure $ (fromRational p) * (r + eo_gamma * (v ! s'))

        put_v s v_s'
        d <- get_delta
        put_delta (d`max`(abs (v_s - v_s')))





