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
  let sum l f = List.sum <$> forM (Set.toList l) f
  let get_delta = gets fst
  let put_delta d = modify (\(_,v) -> (d, v))
  let get_v s = (! s) <$> gets snd
  let put_v s v_s = modify (\(d,v) -> (d, Map.insert s v_s v))

  StateVal . snd <$> do
    flip execStateT (0,v_map) $ loop $ do

      d <- get_delta
      when (d < eo_gamma) $ do
        break ()

      forM_ (rl_states p) $ \s -> do
        v's <- do
          sum (rl_actions p s) $ \a -> do
            sum (rl_transitions p s a) $ \(p, (r, s')) -> do
              v_s' <- get_v s'
              pure $ (fromRational p) * (r + eo_gamma * (v_s'))

        v_s <- get_v s
        put_v s v's
        d <- get_delta
        put_delta (d`max`(abs (v's - v_s)))



{-
  ____      _     _                    _     _
 / ___|_ __(_) __| |_      _____  _ __| | __| |
| |  _| '__| |/ _` \ \ /\ / / _ \| '__| |/ _` |
| |_| | |  | | (_| |\ V  V / (_) | |  | | (_| |
 \____|_|  |_|\__,_| \_/\_/ \___/|_|  |_|\__,_|
-}

data Action = L | R | U | D
  deriving(Show, Eq, Ord, Enum, Bounded)

data GW = GW {
    gw_size :: (Int,Int)
  }
  deriving(Show)

move :: GW -> Point -> Action -> (Reward, Point)
move (GW (sx,sy)) (x,y) a =
  let
    inbound (_, (x',y')) = x' < 0 || x' >= sx || y' < 0 || y' >= sy
    check def perm = if inbound perm then perm else def
  in
  if inbound (0,(x,y)) then
    check (-100, (x,y)) (0,
      case a of
         L -> (x-1,y)
         R -> (x+1,y)
         U -> (x,y-1)
         D -> (x,y+1))
  else
    error "Start state is out of bounds"

instance RLProblem GW (Int,Int) Action where
  rl_states (GW (sx,sy)) = Set.fromList [(x,y) | x <- [0..sx-1], y <- [0..sy-1]]
  rl_actions _ _ = Set.fromList [minBound .. maxBound]
  rl_transitions p@GW{..} s@(x,y) a =
    let
      as = Set.toList $ rl_actions p s
    in
    undefined
    -- FIXME: figure out whats wrong with probabilities
    -- Set.fromList [ (1%(toInteger $ length as),(0.0 , a)) | a <- as ]



