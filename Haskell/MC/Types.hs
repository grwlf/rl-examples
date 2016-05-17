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
module MC.Types where

import Imports
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding(break)

import Types as RL
import DP (DP_Problem(..), DP_Policy(..))
import qualified DP as DP

{-
  ____ _
 / ___| | __ _ ___ ___  ___  ___
| |   | |/ _` / __/ __|/ _ \/ __|
| |___| | (_| \__ \__ \  __/\__ \
 \____|_|\__,_|___/___/\___||___/
-}

class (Fractional num, Ord s) => MC_Problem num pr s a | pr -> s , pr -> a where
  mc_state :: (RandomGen g) => pr num -> g -> (s,g)
  mc_actions :: pr num -> s -> Set a
  mc_transition :: (RandomGen g) => pr num -> s -> a -> g -> (s,g)
  mc_reward :: pr num -> s -> a -> s -> num
  mc_is_terminal :: pr num -> s -> Bool

class (MC_Problem num pr s a) => MC_Policy num pr s a p where
  mcp_action :: (RandomGen g) => pr num -> s -> p -> g -> (Maybe a,g)

class (MC_Problem num pr s a, Show s, Show a, Show (pr num), Show num) => MC_Problem_Show num pr s a

-- Too clumsy. Try using MC_Problem_Show instead
class (MC_Policy num pr s a p, Show s, Show a, Show (pr num), Show num, Show p) => MC_Policy_Show num pr s a p

data MC a s pr num = MC (pr num)
  deriving(Show)

instance DP_Problem num pr s a => MC_Problem num (MC a s pr) s a where
  mc_state (MC pr) = runRand $ uniform (Set.toList (rl_states pr))
  mc_actions (MC pr) = rl_actions pr
  mc_transition (MC pr) s a = runRand $ fromList $ map swap (Set.toList $ rl_transitions pr s a)
  mc_reward (MC pr) = rl_reward pr
  mc_is_terminal (MC pr) s = member s (rl_terminal_states pr)

instance (DP_Problem num pr s a, DP_Policy num p pr s a) => MC_Policy num (MC a s pr) s a p where
  mcp_action (MC pr) s p g =
    case member s (rl_terminal_states pr) of
      True -> (Nothing, g)
      False -> flip runRand g $ fromList $ flip map (Set.toList $ rlp_action p pr s) $ \(p,a) -> (Just a,p)

instance (DP_Policy num p pr s a, Show num, Show a, Show s, Show p, Show (pr num)) => MC_Policy_Show num (MC a s pr) s a p

{-
 _____
|_   _|   _ _ __   ___  ___
  | || | | | '_ \ / _ \/ __|
  | || |_| | |_) |  __/\__ \
  |_| \__, | .__/ \___||___/
      |___/|_|
-}

newtype Episode s a = Episode {
  ep_list :: [(s,a,s)]
  } deriving(Show)

episode_forward Episode{..} = reverse ep_list
episode_backward Episode{..} = ep_list


data EvalOpts num s a = EvalOpts {
    eo_gamma :: num
  -- ^ Forgetness
  -- , eo_etha :: Rational
  -- ^ policy evaluation precision
  , eo_max_iter :: Integer
  -- ^ policy evaluation iteration limit, [1..maxBound]
  -- , eo_floating_precision :: Double
  -- , eo_debug :: (StateVal num s, GenericPolicy s a) -> IO ()
  , eo_learnMonitor :: Maybe (Monitor num s)
  } deriving(Show)

defaultOpts :: (Fractional num) => EvalOpts num s a
defaultOpts = EvalOpts {
    eo_gamma = 0.9
  -- , eo_etha = 0.1
  , eo_max_iter = 10^3
  -- , eo_floating_precision = 1/10^9
  -- , eo_debug = error "no debug specified"
  , eo_learnMonitor = Nothing
  }


data Monitor num s = Monitor {
    mon_target :: StateVal num s
  , mon_data :: PlotData
  } deriving(Show)

monitorNew :: (MonadIO m) => StateVal num s -> m (Monitor num s)
monitorNew tgt = liftIO $
  Monitor tgt <$> do
    newData "MC"


