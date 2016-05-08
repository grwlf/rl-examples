{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Ch_4_GridWorld where

import Imports
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Types as RL
import DP as RL
import MC(MC_Problem(..), MC_Policy(..), MC(..))
import qualified MC as MC
import Prelude hiding (break)

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

data GW num = GW {
    gw_size :: (Int,Int),
    gw_exits :: Set (Int,Int)
  }
  deriving(Show)

gw :: GW num
gw = GW (4,4) (Set.fromList [(0,0),(3,3)])

gw2 :: GW num
gw2 = GW (2,1) (Set.fromList [(1,0)])

instance (Fractional num, Ord num) => DP_Problem num GW (Int,Int) Action where
  rl_states p@(GW (sx,sy) _) = Set.fromList [(x,y) | x <- [0..sx-1], y <- [0..sy-1]]

  rl_actions pr s =
    case Set.member s (rl_terminal_states pr) of
      True -> Set.empty
      False -> Set.fromList [minBound..maxBound]

  rl_transitions (GW (sx,sy) _) (x,y) a =
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

  rl_reward (GW (sx,sy) _) s a s' = -1

  rl_terminal_states (GW _ exits) = exits

data GWRandomPolicy = GWRandomPolicy
  deriving(Show)

instance (Fractional num, Ord num) => DP_Policy num GWRandomPolicy GW (Int,Int) Action where
  rlp_action GWRandomPolicy g s =
    let a = rl_actions g s
    in (\x -> (1%(toInteger $ length a),x))`Set.map`a

showStateVal :: (MonadIO m, Real num) => GW num -> StateVal num Point -> m ()
showStateVal (GW (sx,sy) _) StateVal{..} = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      case Map.lookup (x,y) v_map of
        Just v -> do
          printf "%-2.1f " ((fromRational $ toRational v) :: Double)
        Nothing -> do
          printf "  ?   "
    printf "\n"

showPolicy :: (MonadIO m, DP_Policy num p GW Point Action) => GW num -> p -> m ()
showPolicy pr@(GW (sx,sy) _) p = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      let acts = Set.map snd $ Set.filter (\(pa,a) -> pa > 0) $ rlp_action p pr (x,y)
      printf "% 4s " (showActions acts)
    printf "\n"

example_4_1_dp :: (Fractional num, Ord num, Real num) => GW num -> IO (StateVal num (Int,Int))
example_4_1_dp gw =
  let
    opts = defaultOpts{eo_max_iter=300, eo_gamma = 1, eo_etha = 0.001}
  in do
  v <- policy_eval gw GWRandomPolicy opts (zero_sate_values gw)
  showStateVal gw v
  p' <- policy_improve gw opts v
  showPolicy gw p'
  return v






instance (Fractional num, Ord num) => MC_Problem num GW (Int,Int) Action where
  mc_state p@(GW (sx,sy) _) g =
    flip runRand g $ do
      x <- getRandomR (0,sx-1)
      y <- getRandomR (0,sy-1)
      return (x,y)

  mc_actions pr s =
    case mc_is_terminal pr s of
      True -> Set.empty
      False -> Set.fromList [minBound .. maxBound]

  mc_transition (GW (sx,sy) _) (x,y) a g =
    let
      check (x',y') =
        if x' >= 0 && x' < sx && y' >= 0 && y' < sy then
          (x',y')
        else
          (x,y)
      (_::Integer, g') = random g
    in
    (case a of
       L -> check (x-1,y)
       R -> check (x+1,y)
       U -> check (x,y-1)
       D -> check (x,y+1), g')

  mc_reward (GW (sx,sy) _) s a s' = -1

  mc_is_terminal (GW _ exits) s = Set.member s exits

instance (Fractional num, Ord num) => MC_Policy num GW (Int,Int) Action GWRandomPolicy where
  mcp_action pr s p g =
    case mc_is_terminal pr s of
      True -> (Nothing, g)
      False -> flip runRand g $ Just <$> uniform [minBound .. maxBound]

example_4_1_mc :: (Show num, Fractional num, Ord num, Real num) => GW num -> IO ()
example_4_1_mc gw = do
  let max = 900000
  let g = pureMT 42

  d1 <- newData "mc1"
  d2 <- newData "mc2"

  spawnPlot "plot1" [heredoc|
    set grid back ls 102
    set xrange [0:${show max}]
    set yrange [-20:20]
    set terminal x11 1 noraise
    done = 0
    bind all 'd' 'done = 1'
    while(!done) {
      plot ${dat d1} using 1:2 with lines, ${dat d2} using 1:2 with lines
      pause 1
    }
  |]

  v_dp <- example_4_1_dp gw

  t1 <- forkIO $
    let
      opts = MC.defaultOpts{
               MC.eo_max_iter = max,
               MC.eo_learnMonitor = Just MC.Monitor{
                 mon_target = v_dp,
                 mon_data = d1
               }
             }
    in do
    (v,_) <- MC.policy_eval opts (MC gw) GWRandomPolicy g
    showStateVal gw v

  t2 <- forkIO $
    let
      opts = MC.defaultOpts{
               MC.eo_max_iter = max,
               MC.eo_learnMonitor = Just MC.Monitor{
                 mon_target = v_dp,
                 mon_data = d2
               }
             }
    in do
    (v,_) <- MC.policy_eval opts gw GWRandomPolicy g
    showStateVal gw v

  loop $ do
    c <- liftIO $ getChar
    when (c == 'q')  $ do
        liftIO $ do
          killThread t1
          killThread t2
        break ()


-- FIXME: why are results differ slightly from @example_4_1_mc@ ?
example_4_1_mc2 :: (MonadIO m) => m ()
example_4_1_mc2 = do
  (v,_) <- MC.policy_eval MC.defaultOpts{MC.eo_max_iter = 3000} (MC gw) GWRandomPolicy (mkStdGen 0)
  showStateVal gw v

