{-# LANGUAGE DeriveAnyClass #-}
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
module RL.Q where

import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Binary as HashMap
import qualified Data.HashSet as HashSet
import qualified Control.Lens as Lens
import Control.Monad.Rnd as Rnd
import Prelude hiding (break)

import RL.Imports

{-
  ____ _
 / ___| | __ _ ___ ___  ___  ___
| |   | |/ _` / __/ __|/ _ \/ __|
| |___| | (_| \__ \__ \  __/\__ \
 \____|_|\__,_|___/___/\___||___/
-}

type Q_Number = Double

data Q_Policy = Q_Policy {
    _p_eps :: Q_Number
  } deriving(Show,Read)

$(makeLenses ''Q_Policy)

data Q s a = Q {
    _q_map :: ! (HashMap s (HashMap a Q_Number))
  } deriving(Show,Read,Generic,Binary)

$(makeLenses ''RL.Q.Q)

emptyQ :: Q s a
emptyQ = Q HashMap.empty

mergeQ :: (Eq s, Eq a, Hashable s, Hashable a) => Q s a -> Q s a -> Q s a
mergeQ (view q_map -> q1) (view q_map -> q2) =
  Q $ HashMap.unionWith (HashMap.unionWith (\a b -> (a + b) / 2)) q1 q2

class Q_Problem m pr s sr a | pr -> s , s -> sr, pr -> a, pr -> m where
  q_state :: (MonadRnd g m) => pr -> m s
  -- ^ Extract non-terminal state for the problem @pr@
  -- q_action :: (MonadRnd g m) => pr -> s -> Q_Policy -> Q sr a -> m a
  -- ^ Takes a problem, a state, a policy and a Q(sr,a) , returns the Epsilon-greedy action
  q_transition :: (MonadRnd g m) => pr -> s -> a -> m s
  q_reward :: pr -> s -> a -> s -> Q_Number
  q_is_terminal :: pr -> s -> Bool
  q_state_reduce :: pr -> s -> sr


-- instance (Q_Problem m pr s sr a) => Q_Problem (StateT s m) pr s sr a where
--   q_state = lift . q_state
--   q_transition pr s a = lift $ q_transition pr s a
--   q_reward pr s a s = q_reward pr s a s
--   q_is_terminal :: pr -> s -> Bool
--   q_state_reduce :: pr -> s -> sr

zidx def name = Lens.lens get set where
  get m = case HashMap.lookup name m of
            Just x -> x
            Nothing -> def
  set = (\hs mhv -> HashMap.insert name mhv hs)

q_action :: (Eq sr, Hashable sr,
                  Enum a, Eq a, Hashable a, Bounded a,
                  MonadRnd g m,
                  Q_Problem m pr s sr a)
  => pr -> s -> Q_Policy -> Q sr a -> m a
q_action pr s p q =
  let
    -- g = s^.s_game
    -- hid = s^.s_hid
    -- h = g^.gameHeroes.(idx hid)
    t = q_state_reduce pr s

    {- Stored actions -}
    amap = fromMaybe HashMap.empty $ HashMap.lookup t (q^.q_map)

    {- Map of weighted available actions
     - Use small positive reward to make the agent more qurious
     -}
    qacts =
      flip map [minBound..maxBound] $ \a ->
        let q = fromMaybe 0.01 $ HashMap.lookup a amap
        in (a,q)

    {- Best action -}
    abest = fst $ maximumBy (compare`on`snd) qacts

    {- Rest available actions -}
    arest = map fst $ filter (\x -> fst x /= abest) qacts

  in

  join $ Rnd.fromList [
    swap (toRational $ 1.0-(p^.p_eps), do
      traceM ("best", (1.0-(p^.p_eps)))
      return abest),
    swap (toRational $ p^.p_eps, do
      traceM "random"
      Rnd.uniform arest)
    ]

data Q_State s sr a = Q_State {
    _q_qt :: Q sr a
  -- ^ Q-table
  , _q_s :: s
  -- ^ Final state
  }

$(makeLenses ''Q_State)

data Q_Opts = Q_Opts {
    _q_alpha :: Q_Number
  , _q_gamma :: Q_Number
  , _q_eps :: Q_Number
} deriving (Show)

defaultOpts = Q_Opts {
    _q_alpha = 0.1
  , _q_gamma = 0.5
  , _q_eps = 0.3
  }

$(makeLenses ''Q_Opts)

qlearn :: (Eq sr, Hashable sr,
           Eq a, Hashable a, Bounded a, Enum a, Show a,
           MonadRnd g m, Q_Problem m pr s sr a)
       => pr -> Q_Opts -> Q sr a -> m (Q sr a)
qlearn pr o q0 =
  let
    hmax hm =
      case HashMap.toList hm of
        [] -> 0
        x -> snd $ maximumBy (compare`on`snd) x

    alpha = o^.q_alpha
    gamma = o^.q_gamma
    p = Q_Policy (o^.q_eps)
  in do
  s0 <- q_state pr
  view q_qt <$> do
  flip execStateT (Q_State q0 s0) $ do
  loop $ do
    s <- use q_s
    a <- use q_qt >>= \q -> lift $ lift (q_action pr s p q)
    -- traceM a
    s' <- lift $ lift $ q_transition pr s a
    when (q_is_terminal pr s') $ do
      break ()

    r <- pure $ q_reward pr s a s'
    t <- pure $ q_state_reduce pr s
    t' <- pure $ q_state_reduce pr s'

    q'max <- hmax <$> use (q_qt.q_map.(zidx mempty t'))

    q_qt.q_map.(zidx mempty t).(zidx 0 a) %= (\q -> q + alpha*(r + gamma*q'max - q))
    q_s %= const s'


qexec :: (Eq sr, Hashable sr,
           Eq a, Hashable a, Bounded a, Enum a, Show a,
           MonadRnd g m, Q_Problem m pr s sr a)
       => pr -> Q_Opts -> Q sr a -> m ()
qexec pr o q0 =
  let
    alpha = o^.q_alpha
    gamma = o^.q_gamma
    p = Q_Policy 0.0
  in do
  s0 <- q_state pr
  flip evalStateT (Q_State q0 s0) $ do
  loop $ do
    s <- use q_s
    a <- use q_qt >>= \q -> lift $ lift (q_action pr s p q)
    s' <- lift $ lift $ q_transition pr s a
    when (q_is_terminal pr s') $ do
      break ()

    q_s %= const s'



