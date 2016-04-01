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
  rl_actions :: p -> s -> Set (Probability, a) -- policy
  rl_transitions :: p -> s -> a -> Set (Probability, (Reward, s))


rl_prob_invariant :: forall p s a . (RLProblem p s a) => p -> s -> a -> Bool
rl_prob_invariant p s a = 1%1 == List.sum (map fst (Set.toList $ rl_transitions p s a))


data Policy s a = Policy {
    pol_map :: Map s a
  } deriving(Show)

policy_init :: forall p s a m . (RLProblem p s a)
  => p -> StateVal s
policy_init p =  StateVal $ Map.fromList $ map (\s -> (s,0.0)) (Set.toList $ rl_states p)

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
    es_delta :: Double
  , es_v :: Map s Double
  , es_v' :: Map s Double
  , es_iter :: Int
  } deriving(Show)

initEvalState StateVal{..} = EvalState 0.0 v_map v_map 0

policy_eval :: forall p s a m . (RLProblem p s a, MonadIO m)
  => p -> EvalOpts -> StateVal s -> m (StateVal s)
policy_eval p EvalOpts{..} v = do
  let sum l f = List.sum <$> forM (Set.toList l) f
  let get_delta = gets es_delta
  let put_delta d = modify $ \e@EvalState{..} -> e{es_delta = d}
  let get_v s = (! s) <$> gets es_v
  let put_v s v_s = modify $ \e@EvalState{..} -> e{es_v' = Map.insert s v_s es_v'}

  StateVal . es_v <$> do
    flip execStateT (initEvalState v) $ loop $ do

      i <- gets es_iter
      when (i > eo_max_iter-1) $ do
        break ()

      -- debug $ "iter: " ++ show i
      put_delta 0.0

      forM_ (rl_states p) $ \s -> do
        v's <- do
          sum (rl_actions p s) $ \(pi, a) -> do
            ((fromRational pi)*) <$> (do
              sum (rl_transitions p s a) $ \(p, (r, s')) -> do
                v_s' <- get_v s'
                pure $ (fromRational p) * (r + eo_gamma * (v_s')))

        v_s <- get_v s
        put_v s v's
        d <- get_delta
        put_delta (d`max`(abs (v's - v_s)))

      d <- get_delta
      -- debug $ show d
      when (d < eo_etha) $ do
        break ()


      modify $ \s -> s{ es_iter = i + 1 }
      modify $ \s -> s{ es_v = es_v' s }



{-
  ____      _     _                    _     _
 / ___|_ __(_) __| |_      _____  _ __| | __| |
| |  _| '__| |/ _` \ \ /\ / / _ \| '__| |/ _` |
| |_| | |  | | (_| |\ V  V / (_) | |  | | (_| |
 \____|_|  |_|\__,_| \_/\_/ \___/|_|  |_|\__,_|
-}

data Action = L | R | U | D
  deriving(Show, Eq, Ord, Enum, Bounded)

actions :: [Action]
actions = [minBound .. maxBound]

data GW = GW {
    gw_size :: (Int,Int)
  }
  deriving(Show)

gw = GW (4,4)

move :: GW -> Point -> Action -> (Reward, Point)
move (GW (sx,sy)) (x,y) a =
  let
    inbound (x',y') = x' >= 0 && x' < sx && y' >= 0 && y' < sy
    check def (r,perm) = if inbound perm then (r,perm) else def
  in
  if inbound (x,y) then
    check (-1, (x,y)) (-1,
      case a of
         L -> (x-1,y)
         R -> (x+1,y)
         U -> (x,y-1)
         D -> (x,y+1))
  else
    error $ "Start state is out of bounds: " ++ show (x,y)

instance RLProblem GW (Int,Int) Action where
  rl_states p@(GW (sx,sy)) = Set.fromList [(x,y) | x <- [0..sx-1], y <- [0..sy-1]]
  rl_actions (GW (sx,sy)) s =
    case s == (0,0) || s == (sx-1,sy-1) of
      True -> Set.empty
      False -> Set.fromList [ (1%(toInteger $ length actions),a) | a <- actions]
  rl_transitions p@GW{..} s@(x,y) a = Set.fromList [(1%1, move p s a)]


showStateVal :: (MonadIO m) => GW -> StateVal Point -> m ()
showStateVal (GW (sx,sy)) StateVal{..} = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      printf "%-2.1f . " (v_map ! (x,y))
    printf "\n"

test_eval :: IO ()
test_eval = showStateVal gw =<< policy_eval gw defaultOpts{eo_max_iter=300, eo_gamma = 1, eo_etha = 0.001} (policy_init gw)

