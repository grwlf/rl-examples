{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

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

import Types as TE
import DP as TE

{-
  ____      _     _                    _     _
 / ___|_ __(_) __| |_      _____  _ __| | __| |
| |  _| '__| |/ _` \ \ /\ / / _ \| '__| |/ _` |
| |_| | |  | | (_| |\ V  V / (_) | |  | | (_| |
 \____|_|  |_|\__,_| \_/\_/ \___/|_|  |_|\__,_|

 Example 4.1, pg.86
-}


type Point = (Int,Int)

data Action = L | R | U | D
  deriving(Show, Eq, Ord, Enum, Bounded)

actions :: [Action]
actions = [minBound .. maxBound]

showAction :: Action -> String
showAction a =
  case a of
    L->"<"
    R->">"
    U->"^"
    D->"v"

showActions :: Set Action -> String
showActions = concat . map showAction . List.sort . Set.toList

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
  rl_actions p@(GW (sx,sy)) s@(x,y) =
    case s == (0,0) || s == (sx-1,sy-1) of
      True -> Set.empty
      False -> Set.fromList actions
  rl_transitions p@GW{..} s@(x,y) a = Set.fromList [(1%1, move p s a)]

data GWRandomPolicy = GWRandomPolicy
  deriving(Show)

instance RLPolicy GWRandomPolicy GW (Int,Int) Action where
  rlp_action GWRandomPolicy g s =
    let a = rl_actions g s
    in (\x -> (1%(toInteger $ length a),x))`Set.map`a

showStateVal :: (MonadIO m) => GW -> StateVal Point -> m ()
showStateVal (GW (sx,sy)) StateVal{..} = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      printf "%-2.1f " (fromRational $ v_map ! (x,y) :: Double)
    printf "\n"

showPolicy :: (MonadIO m, RLPolicy p GW Point Action) => GW -> p -> m ()
showPolicy pr@(GW (sx,sy)) p = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      let acts = Set.map snd $ Set.filter (\(pa,a) -> pa > 0) $ rlp_action p pr (x,y)
      printf "% 4s " (showActions acts)
    printf "\n"

example_4_1 :: IO ()
example_4_1 =
  let
    opts = defaultOpts{eo_max_iter=300, eo_gamma = 1, eo_etha = 0.001}
  in do
  v <- policy_eval gw GWRandomPolicy opts (zero_sate_values gw)
  showStateVal gw v
  p' <- policy_improve gw opts v
  showPolicy gw p'

