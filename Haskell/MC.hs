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
module MC where

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

class (Ord s) => MC_Problem pr s a | pr -> s , pr -> a where
  mc_state :: (RandomGen g) => pr -> g -> (s,g)
  mc_actions :: pr -> s -> Set a
  mc_transition :: (RandomGen g) => pr -> s -> a -> g -> (s,g)
  mc_reward :: pr -> s -> a -> s -> Reward
  mc_is_terminal :: pr -> s -> Bool

class (MC_Problem pr s a) => MC_Policy pr s a p where
  mcp_action :: (RandomGen g) => pr -> s -> p -> g -> (Maybe a,g)

data MC pr s a = MC pr
  deriving(Show)

instance DP_Problem pr s a => MC_Problem (MC pr s a) s a where
  mc_state (MC pr) = runRand $ uniform (Set.toList (rl_states pr))
  mc_actions (MC pr) = rl_actions pr
  mc_transition (MC pr) s a = runRand $ fromList $ map swap (Set.toList $ rl_transitions pr s a)
  mc_reward (MC pr) = rl_reward pr
  mc_is_terminal (MC pr) s = member s (rl_terminal_states pr)

instance (DP_Problem pr s a, DP_Policy p pr s a) => MC_Policy (MC pr s a) s a p where
  mcp_action (MC pr) s p g =
    case member s (rl_terminal_states pr) of
      True -> (Nothing, g)
      False -> flip runRand g $ fromList $ flip map (Set.toList $ rlp_action p pr s) $ \(p,a) -> (Just a,p)


{-
    _    _
   / \  | | __ _
  / _ \ | |/ _` |
 / ___ \| | (_| |
/_/   \_\_|\__, |
           |___/
-}


newtype Episode s a = Episode {
  ep_list :: [(s,a,s)]
  } deriving(Show)

episode_forward Episode{..} = reverse ep_list
episode_backward Episode{..} = ep_list

-- | Builds an episode which is a list of transitions, terminal transition goes in head
episode :: (RandomGen g , MC_Policy pr s a p, MonadIO m, Show s, Show a) => pr -> s -> p -> g -> m (Episode s a,g)
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
        Just a <- rnd $ mcp_action pr s p
        s' <- rnd $ mc_transition pr s a
        -- liftIO $ putStrLn $ ". " ++  show (s,a,s')
        modify $ const s' *** ((s,a,s'):)

-- Backtrack rewards, first visit counts
backtrack_fv :: (MC_Problem pr s a) => pr -> Episode s a -> Map s Rational
backtrack_fv pr ep =
  view _1 $ flip execState (Map.empty, 0%1) $ do
    forM_ (episode_backward ep) $ \ (s,a,s') -> do
      r <- pure $ mc_reward pr s a s'
      _2 %= (+r)
      g <- use _2
      _1 %= (Map.insert s g)


data Monitor s = Monitor {
    mon_target :: StateVal s
  , mon_data :: PlotData
  } deriving(Show)

monitorNew :: (MonadIO m) => StateVal s -> m (Monitor s)
monitorNew tgt = liftIO $
  Monitor tgt <$> do
    newData "MC"


data EvalOpts s a = EvalOpts {
    eo_gamma :: Rational
  -- ^ Forgetness
  -- , eo_etha :: Rational
  -- ^ policy evaluation precision
  , eo_max_iter :: Integer
  -- ^ policy evaluation iteration limit, [1..maxBound]
  -- , eo_floating_precision :: Double
  -- , eo_debug :: (StateVal s, GenericPolicy s a) -> IO ()
  , eo_learnMonitor :: Maybe (Monitor s)
  } deriving(Show)

defaultOpts = EvalOpts {
    eo_gamma = 0.9
  -- , eo_etha = 0.1
  , eo_max_iter = 10^3
  -- , eo_floating_precision = 1/10^9
  -- , eo_debug = error "no debug specified"
  , eo_learnMonitor = Nothing
  }

data Avg num = Avg {
    avg_curr :: num
  , avg_n :: num
  } deriving(Show)

initialAvg :: (Fractional num) => Avg num
initialAvg = Avg 0 0

current :: (Fractional s) => Avg s -> s
current (Avg c n) = c

meld :: forall s . (Fractional s) => Avg s -> s -> Avg s
meld (Avg c n) s = Avg (c + (s-c)/(n+1)) (n + 1)

-- testAvg :: Double
testAvg x = do
  -- fromRational $ do
  (current *** (\l -> sum l / (fromInteger $ toInteger $ length l))) $ do
  fst $ flip runRand (mkStdGen 0) $ do
  flip execStateT (initialAvg :: Avg Double, ([] :: [Double])) $ do
  forM_ [0..x] $ \i -> do
    -- r <- getRandomR (1,9)
    modify $ (flip meld (fromInteger i)) *** (fromInteger i:)


data EvalState s = EvalState {
    _es_v :: Map s (Avg Rational)
  , _es_iter :: Integer
  } deriving(Show)

makeLenses ''EvalState

initialEvalState :: (Ord s) => EvalState s
initialEvalState = EvalState mempty 0

-- | DIfference between state value estimates
-- FIXME: handle missing states case
diffVal :: (Ord s) => StateVal s -> (Map s (Avg Rational)) -> Rational
diffVal (v_map -> tgt) src = sum $ Map.intersectionWith (\a b -> abs $ a - (current b)) tgt src


-- Monte carlo policy evaluation, Figure 5.1. pg 109
policy_eval :: forall p pr s a m g . (MC_Policy pr s a p, RandomGen g, MonadIO m, Show s, Show a)
  => EvalOpts s a -> pr -> p -> g -> m (StateVal s, g)
policy_eval EvalOpts{..} pr p g = do
  flip runRandT g $ do
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
      mv <- uses es_v (Map.lookup s)
      case mv of
        Just v -> do
          {- Melding new estimates with all the previous ones -}
          es_v %= (Map.insert s (meld v g))
        Nothing -> do
          {- Act as V(s) is 0. Initialize it with current reward -}
          es_v %= (Map.insert s (meld initialAvg g))

    case eo_learnMonitor of
      Nothing -> return ()
      Just Monitor{..} -> do
        v <- use es_v
        push mon_data (fromInteger i) (fromRational $ diffVal mon_target v)


