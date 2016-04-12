{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
module Types where

import Control.Monad
import Control.Monad.Trans
import Data.Ratio
import Data.List hiding (break)
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

debug :: (MonadIO m) => String -> m ()
debug = liftIO . putStrLn

type Probability = Rational
type Reward = Rational

data StateVal s = StateVal {
    v_map :: Map s Rational
  } deriving(Show)

zero_sate_values :: forall pr s a m . (RLProblem pr s a)
  => pr -> StateVal s
zero_sate_values pr =  StateVal $ Map.fromList $ map (\s -> (s,0.0)) (Set.toList $ rl_states pr)


class (Ord s) => RLProblem pr s a | pr -> s , pr -> a where
  rl_states :: pr -> Set s
  rl_actions :: pr -> s -> Set a
  rl_transitions :: pr -> s -> a -> Set (Probability, s)
  rl_reward :: pr -> s -> a -> s -> Reward

invariant_prob :: forall pr s a . (RLProblem pr s a) => pr -> s -> a -> Bool
invariant_prob pr s a = 1%1 == List.sum (map fst (Set.toList $ rl_transitions pr s a))

class (RLProblem pr s a) => RLPolicy p pr s a where
  rlp_action :: p -> pr -> s -> Set (Probability, a)

invariant1 :: (Monad m, RLProblem pr s a, Show s, Show a, Show pr) => pr -> m ()
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

policy_eq :: (Eq a, RLPolicy p1 pr s a, RLPolicy p2 pr s a) => pr -> p1 -> p2 -> Bool
policy_eq pr p1 p2 = all (\s -> (rlp_action p1 pr s) == (rlp_action p2 pr s)) (rl_states pr)

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


