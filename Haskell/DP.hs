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

class (Ord s) => RLProblem pr s a | pr -> s , pr -> a where
  rl_states :: pr -> Set s
  rl_actions :: pr -> s -> Set a
  rl_transitions :: pr -> s -> a -> Set (Probability, (Reward, s))

rl_prob_invariant :: forall p s a . (RLProblem p s a) => p -> s -> a -> Bool
rl_prob_invariant p s a = 1%1 == List.sum (map fst (Set.toList $ rl_transitions p s a))

class (RLProblem pr s a) => RLPolicy p pr s a where
  rlp_action :: p -> pr -> s -> Set (Probability, a)

policy_eq :: (Eq a, RLPolicy p1 pr s a, RLPolicy p2 pr s a) => pr -> p1 -> p2 -> Bool
policy_eq pr p1 p2 = all (\s -> (rlp_action p1 pr s) == (rlp_action p2 pr s)) (rl_states pr)

zero_sate_values :: forall pr s a m . (RLProblem pr s a)
  => pr -> StateVal s
zero_sate_values pr =  StateVal $ Map.fromList $ map (\s -> (s,0.0)) (Set.toList $ rl_states pr)

data EvalOpts = EvalOpts {
    eo_gamma :: Double
  -- ^ Forgetness
  , eo_etha :: Double
  -- ^ policy evaluation precision
  , eo_max_iter :: Int
  -- ^ policy evaluation iteration limit, [1..maxBound]
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

uniformGenericPolicy :: (Ord a, RLProblem pr s a) => pr -> GenericPolicy s a
uniformGenericPolicy pr = GenericPolicy{..} where
  gp_actions = Map.fromList $ map (\s ->
    let
      as = rl_actions pr s
    in
    (s, Set.map (\a -> (1%(toInteger $ length as),a)) as)) (Set.toList $ rl_states pr)


-- FIXME:check
policy_improve :: forall pol pr s a m . (RLProblem pr s a, MonadIO m, Ord a)
  => pr -> EvalOpts -> StateVal s -> m (GenericPolicy s a)
policy_improve pr EvalOpts{..} StateVal{..} = do
  let sum l f = List.sum <$> forM (Set.toList l) f

  GenericPolicy <$> do
    flip execStateT Map.empty $ do

      forM_ (rl_states pr) $ \s -> do
        (maxv, maxa) <- do
          foldlM (\(val,maxa) a -> do
                    pi_s <- do
                      sum (rl_transitions pr s a) $ \(fromRational -> p, (r, s')) -> do
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
                 ) (0.0 ,Set.empty) (rl_actions pr s)

        let nmax = toInteger (Set.size maxa)
        modify $ Map.insert s (Set.map (\a -> (1%nmax,a)) maxa)


policy_iteraton_step :: forall pr p s a m . (RLPolicy p pr s a, MonadIO m, Ord a)
  => pr -> p -> StateVal s -> EvalOpts -> m (StateVal s, GenericPolicy s a)
policy_iteraton_step pr p v eo = do
  v' <- policy_eval pr p eo v
  p' <- policy_improve pr eo v'
  return (v',p')

data PolicyContainer p s a = APolicy p | GPolicy (GenericPolicy s a)

withAnyPolicy :: forall p pr s a m x . (RLPolicy p pr s a, Monad m)
  => pr -> PolicyContainer p s a -> (forall p1 . RLPolicy p1 pr s a => p1 -> m x) -> m x
withAnyPolicy pr ap handler = do
  case ap of
    APolicy p -> handler p
    GPolicy gp -> handler gp

policy_iteraton :: forall pr p s a m . (RLPolicy p pr s a, MonadIO m, Ord a)
  => pr -> p -> StateVal s -> EvalOpts -> m (GenericPolicy s a)
policy_iteraton pr p v eo = do
  (_, GPolicy p) <- flip execStateT (v, APolicy p) $ loop $ do
    (v,ap) <- get
    withAnyPolicy pr ap $ \p -> do
        (v', p') <- policy_iteraton_step pr p v eo
        -- liftIO $ dbg p'
        put (v', GPolicy p')
        when (policy_eq pr p p') $ do
          break ()
  return p


