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

import Types as RL

{-
  ____ _
 / ___| | __ _ ___ ___  ___  ___
| |   | |/ _` / __/ __|/ _ \/ __|
| |___| | (_| \__ \__ \  __/\__ \
 \____|_|\__,_|___/___/\___||___/
-}

zero_sate_values :: forall pr s a m . (DP_Problem pr s a)
  => pr -> StateVal s
zero_sate_values pr =  StateVal $ Map.fromList $ map (\s -> (s,0.0)) (Set.toList $ rl_states pr)

-- FIXME: Convert to fold-like style
class (Ord s) => DP_Problem pr s a | pr -> s , pr -> a where
  rl_states :: pr -> Set s
  rl_actions :: pr -> s -> Set a
  rl_transitions :: pr -> s -> a -> Set (Probability, s)
  rl_reward :: pr -> s -> a -> s -> Reward

invariant_prob :: forall pr s a . (DP_Problem pr s a) => pr -> s -> a -> Bool
invariant_prob pr s a = 1%1 == List.sum (map fst (Set.toList $ rl_transitions pr s a))

class (DP_Problem pr s a) => DP_Policy p pr s a where
  rlp_action :: p -> pr -> s -> Set (Probability, a)

invariant1 :: (Monad m, DP_Problem pr s a, Show s, Show a, Show pr) => pr -> m ()
invariant1 pr = do
  forM_ (rl_states pr) $ \s -> do
    forM_ (rl_actions pr s) $ \a -> do
      case Set.toList $ rl_transitions pr s a of
        [] -> return ()
        xs -> do
          when (not $ invariant_prob pr s a) $ do
            fail $ "State " ++ show s ++ ", action " ++ show a ++ ": probabilities don't sumup to 1"
      forM_ (rl_transitions pr s a) $ \(p, s') -> do
        when (not $ Set.member s' (rl_states pr)) $ do
          fail $ "State " ++ show s ++ ", action " ++ show a ++ ": lead to invalid state " ++ show s'

policy_eq :: (Eq a, DP_Policy p1 pr s a, DP_Policy p2 pr s a) => pr -> p1 -> p2 -> Bool
policy_eq pr p1 p2 = all (\s -> (rlp_action p1 pr s) == (rlp_action p2 pr s)) (rl_states pr)

instance (DP_Problem p s a) => DP_Policy (GenericPolicy s a) p s a where
  rlp_action GenericPolicy{..} _ s = gp_actions ! s

uniformGenericPolicy :: (Ord a, DP_Problem pr s a) => pr -> GenericPolicy s a
uniformGenericPolicy pr = GenericPolicy{..} where
  gp_actions = Map.fromList $ map (\s ->
    let
      as = rl_actions pr s
    in
    (s, Set.map (\a -> (1%(toInteger $ length as),a)) as)) (Set.toList $ rl_states pr)



{-
    _    _
   / \  | | __ _
  / _ \ | |/ _` |
 / ___ \| | (_| |
/_/   \_\_|\__, |
           |___/
-}


data EvalOpts s a = EvalOpts {
    eo_gamma :: Rational
  -- ^ Forgetness
  , eo_etha :: Rational
  -- ^ policy evaluation precision
  , eo_max_iter :: Int
  -- ^ policy evaluation iteration limit, [1..maxBound]
  -- , eo_floating_precision :: Double

  , eo_debug :: (StateVal s, GenericPolicy s a) -> IO ()
  } --deriving(Show)

defaultOpts = EvalOpts {
    eo_gamma = 0.9
  , eo_etha = 0.1
  , eo_max_iter = 10^3
  -- , eo_floating_precision = 1/10^9
  , eo_debug = error "no debug specified"
  }

data EvalState s = EvalState {
    _es_delta :: Rational
  , _es_v :: Map s Rational
  , _es_v' :: Map s Rational
  , _es_iter :: Int
  } deriving(Show)

makeLenses ''EvalState

initEvalState StateVal{..} = EvalState 0 v_map v_map 0

-- | Iterative policy evaluation algorithm
-- Figure 4.1, pg.86.
policy_eval :: forall p pr s a m . (DP_Policy p pr s a, MonadIO m)
  => pr -> p -> EvalOpts s a -> StateVal s -> m (StateVal s)
policy_eval pr p EvalOpts{..} v = do
  let sum l f = List.sum <$> forM (Set.toList l) f

  StateVal . view es_v <$> do
    flip execStateT (initEvalState v) $ loop $ do

      i <- use es_iter
      when (i > eo_max_iter-1) $ do
        break ()

      es_delta %= const 0

      forM_ (rl_states pr) $ \s -> do
        v_s <- uses es_v (!s)
        v's <- do
          sum (rlp_action p pr s) $ \(fromRational -> pa, a) -> do
            (pa*) <$> do
              sum (rl_transitions pr s a) $ \(fromRational -> p, s') -> do
                v_s' <- uses es_v (!s')
                pure $ p * ((rl_reward pr s a s') + eo_gamma * (v_s'))

        es_v' %= (Map.insert s v's)
        es_delta %= (`max`(abs (v's - v_s)))

      d <- use es_delta
      when (d < eo_etha) $ do
        break ()

      v' <- use es_v'
      es_v %= const v'

      es_iter %= (+1)

policy_action_value pr s a EvalOpts{..} StateVal{..} =
  List.sum $
  flip map (Set.toList $ rl_transitions pr s a) $ \(fromRational -> p, s') ->
    p * ((rl_reward pr s a s') + eo_gamma * (v_map ! s'))

policy_improve :: forall p pr s a m . (DP_Problem pr s a, MonadIO m, Ord a)
  => pr -> EvalOpts s a -> StateVal s -> m (GenericPolicy s a)
policy_improve pr eo@EvalOpts{..} v@StateVal{..} = do
  let sum l f = List.sum <$> forM (Set.toList l) f

  GenericPolicy <$> do
    flip execStateT Map.empty $ do

      forM_ (rl_states pr) $ \s -> do
        (maxv, maxa) <- do
          foldlM (\(val,maxa) a -> do
                    pi_s <- pure $ policy_action_value pr s a eo v
                    return $
                      if Set.null maxa then
                        (pi_s, Set.singleton a)
                      else
                        if pi_s > val then
                          -- GT
                          (pi_s, Set.singleton a)
                        else
                          if pi_s < val then
                            --LT
                            (val,maxa)
                          else
                            -- EQ
                            (val, Set.insert a maxa)
                 ) (0 ,Set.empty) (rl_actions pr s)

        let nmax = toInteger (Set.size maxa)
        modify $ Map.insert s (Set.map (\a -> (1%nmax,a)) maxa)


policy_iteraton_step :: forall p pr s a m . (DP_Policy p pr s a, MonadIO m, Ord a)
  => pr -> p -> StateVal s -> EvalOpts s a -> m (StateVal s, GenericPolicy s a)
policy_iteraton_step pr p v eo = do
  v' <- policy_eval pr p eo v
  p' <- policy_improve pr eo v'
  return (v',p')

data PolicyContainer p s a = APolicy p | GPolicy (GenericPolicy s a)

withAnyPolicy :: forall p pr s a m x . (DP_Policy p pr s a, Monad m)
  => pr -> PolicyContainer p s a -> (forall p1 . DP_Policy p1 pr s a => p1 -> m x) -> m x
withAnyPolicy pr ap handler = do
  case ap of
    APolicy p -> handler p
    GPolicy gp -> handler gp

policy_iteraton :: forall pr p s a m . (DP_Policy p pr s a, MonadIO m, Ord a)
  => pr -> p -> StateVal s -> EvalOpts s a -> m (StateVal s, GenericPolicy s a)
policy_iteraton pr p v eo = do
  (v', GPolicy p') <- flip execStateT (v, APolicy p) $ loop $ do
    (v,ap) <- get
    withAnyPolicy pr ap $ \p -> do
        (v', p') <- policy_iteraton_step pr p v eo
        liftIO $ eo_debug eo (v', p')
        put (v', GPolicy p')
        when (policy_eq pr p p') $ do
          break ()
  return (v',p')


