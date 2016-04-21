{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Ch_4_GridWorld where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ratio
import Text.Printf

import Types as RL
import DP as RL
import MC(MC_Problem(..), MC_Policy(..))
import qualified MC as MC

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

instance DP_Problem GW (Int,Int) Action where
  rl_states p@(GW (sx,sy)) = Set.fromList [(x,y) | x <- [0..sx-1], y <- [0..sy-1]]

  rl_actions p@(GW (sx,sy)) s@(x,y) =
    case s == (0,0) || s == (sx-1,sy-1) of
      True -> Set.empty
      False -> Set.fromList [minBound..maxBound]

  rl_transitions (GW (sx,sy)) (x,y) a =
    let
      check (x',y') =
        if x' >= 0 && x' < sx && y' >= 0 && y' < sy then
          (x',y')
        else
          (x,y)
    in
    Set.fromList [(1%1,
        case a of
           L -> check (x-1,y)
           R -> check (x+1,y)
           U -> check (x,y-1)
           D -> check (x,y+1)
    )]

  rl_reward (GW (sx,sy)) s a s' = -1


data GWRandomPolicy = GWRandomPolicy
  deriving(Show)

instance DP_Policy GWRandomPolicy GW (Int,Int) Action where
  rlp_action GWRandomPolicy g s =
    let a = rl_actions g s
    in (\x -> (1%(toInteger $ length a),x))`Set.map`a

showStateVal :: (MonadIO m) => GW -> StateVal Point -> m ()
showStateVal (GW (sx,sy)) StateVal{..} = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      case Map.lookup (x,y) v_map of
        Just x -> do
          printf "%-2.1f " (fromRational $ x :: Double)
        Nothing -> do
          printf "  ?   "
    printf "\n"

showPolicy :: (MonadIO m, DP_Policy p GW Point Action) => GW -> p -> m ()
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






instance MC_Problem GW (Int,Int) Action where
  mc_state p@(GW (sx,sy)) g =
    flip runRand g $ do
      x <- getRandomR (0,sx-1)
      y <- getRandomR (0,sy-1)
      return (x,y)

  mc_actions pr s =
    case mc_is_terminal pr s of
      True -> Set.empty
      False -> Set.fromList [minBound .. maxBound]

  mc_transition (GW (sx,sy)) (x,y) a g =
    let
      check (x',y') =
        if x' >= 0 && x' < sx && y' >= 0 && y' < sy then
          (x',y')
        else
          (x,y)
    in
    (case a of
       L -> check (x-1,y)
       R -> check (x+1,y)
       U -> check (x,y-1)
       D -> check (x,y+1), g)

  mc_reward (GW (sx,sy)) s a s' = -1

  mc_is_terminal (GW (sx,sy)) s = (s == (0,0)) || (s == (sx-1,sy-1))


instance MC_Policy GW (Int,Int) Action GWRandomPolicy where
  mcp_action pr s p g =
    case mc_is_terminal pr s of
      True -> (Nothing, g)
      False -> flip runRand g $ Just <$> uniform [minBound .. maxBound]



example_4_1_mc :: (MonadIO m) => m ()
example_4_1_mc = do
  (v,_) <- MC.policy_eval MC.defaultOpts{MC.eo_max_iter = 10000} gw GWRandomPolicy (mkStdGen 0)
  showStateVal gw v

