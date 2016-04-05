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

module Ch_4_GridWorld where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ratio
import Text.Printf

import DP

{-
  ____      _     _                    _     _
 / ___|_ __(_) __| |_      _____  _ __| | __| |
| |  _| '__| |/ _` \ \ /\ / / _ \| '__| |/ _` |
| |_| | |  | | (_| |\ V  V / (_) | |  | | (_| |
 \____|_|  |_|\__,_| \_/\_/ \___/|_|  |_|\__,_|

 Example 4.1, pg.86
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
  rl_transitions p@GW{..} s@(x,y) a = Set.fromList [(1%1, move p s a)]

data GWRandomPolicy = GWRandomPolicy
  deriving(Show)

instance RLPolicy GWRandomPolicy GW (Int,Int) Action where
  rlp_action GWRandomPolicy (GW (sx,sy)) s =
    case s == (0,0) || s == (sx-1,sy-1) of
      True -> Set.empty
      False -> Set.fromList [ (1%(toInteger $ length actions),a) | a <- actions]


showStateVal :: (MonadIO m) => GW -> StateVal Point -> m ()
showStateVal (GW (sx,sy)) StateVal{..} = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      printf "%-2.1f . " (v_map ! (x,y))
    printf "\n"

example_4_1 :: IO ()
example_4_1 = showStateVal gw =<< policy_eval gw GWRandomPolicy opts (zero_sate_values gw) where
  opts = defaultOpts{eo_max_iter=300, eo_gamma = 1, eo_etha = 0.001}

