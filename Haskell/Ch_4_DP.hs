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

import Control.Monad
import Control.Monad.Trans
import Data.Ratio
import Data.List as List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

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
  rl_transitions :: p -> s -> a -> Set (Probability, Reward, s)

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
  let sum l f = do
        x <- forM (Set.toList l) f
        return (List.sum x)

  v' <- forM (Set.toList $ rl_states p) $ \s -> do
    temp <- pure $ v ! s
    v_s <- sum (rl_actions p s) $ \a -> do
              sum (rl_transitions p s a) $ \(p, r, s') -> do
                return $ (fromRational p) * (r + eo_gamma * (v ! s'))
    return (v_s, temp - v_s)

  return (StateVal v')

  undefined




