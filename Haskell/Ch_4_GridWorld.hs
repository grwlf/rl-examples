{-# LANGUAGE NondecreasingIndentation #-}
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
import DP as RL hiding(eo_debug)
import MC(MC_Problem(..), MC_Policy(..), MC_Policy_Show(..), MC_Problem_Show(..), Opts(..))
import qualified MC
import MC (E_Ext(..))
import qualified MC.ES
import MC.ES (ES_Ext(..), ES_State(..))

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

{- DP instances -}
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
    Set.fromList [(
        case a of
           L -> check (x-1,y)
           R -> check (x+1,y)
           U -> check (x,y-1)
           D -> check (x,y+1)
        , 1%1)]

  rl_reward (GW (sx,sy) _) s a s' = -1

  rl_terminal_states (GW _ exits) = exits

data GWRandomPolicy = GWRandomPolicy
  deriving(Show)

instance (Fractional num, Ord num) => DP_Policy num GWRandomPolicy GW (Int,Int) Action where
  rlp_action GWRandomPolicy g s =
    let a = rl_actions g s
    in (\x -> (x,1%(toInteger $ length a)))`Set.map`a

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

showGenericPolicy :: (MonadIO m,
                      DP_Policy num (GenericPolicy Point Action) GW Point Action)
  => GW num
  -> GenericPolicy Point Action
  -> m ()
showGenericPolicy pr@(GW (sx,sy) _) p@GenericPolicy{..} = liftIO $ do
  forM_ [0..sy-1] $ \y -> do
    forM_ [0..sx-1] $ \x -> do
      case Map.lookup (x,y) (view p_map p) of
        Nothing ->  do
          printf "  ?   "
        Just ap -> do
          let acts = Set.map fst $ Set.filter (\(a,pa) -> pa > 0) ap
          printf "% 4s " (showActions acts)
    printf "\n"

-- DP approach
example_4_1_dp :: (Fractional num, Ord num, Real num) => GW num -> IO (StateVal num (Int,Int))
example_4_1_dp gw =
  let
    opts = defaultOpts{eo_max_iter=300, eo_gamma = 1, eo_etha = 0.001}
  in do
  v <- policy_eval gw GWRandomPolicy opts (zero_sate_values gw)
  showStateVal gw v
  p' <- policy_improve gw opts v
  showGenericPolicy gw p'
  return v





{- MC instances -}

instance (Fractional num, Ord num) => MC_Problem num GW (Int,Int) Action where

  mc_state_nonterm gw@(GW (sx,sy) exits) g =
    let
      (p,g') = flip runRand g $ do
            x <- getRandomR (0,sx-1)
            y <- getRandomR (0,sy-1)
            return (x,y)
    in
      -- FIXME: try to replace recursion with direct selection
      case p `member` exits of
        True -> mc_state_nonterm gw g'
        False -> (p,g')

  mc_actions pr s = Set.fromList [minBound .. maxBound]

  mc_transition (GW (sx,sy) exits) (x,y) a g =
    let
      check (x',y') =
        if x' >= 0 && x' < sx && y' >= 0 && y' < sy then
          (x',y')
        else
          (x,y)
      (_::Integer, g') = random g

      p' = case a of
                 L -> check (x-1,y)
                 R -> check (x+1,y)
                 U -> check (x,y-1)
                 D -> check (x,y+1)
      term = p' `member` exits
    in
    ((p',term), g')

  mc_reward (GW (sx,sy) _) s a s' = -1

instance (Fractional num, Ord num) => MC_Policy num GW (Int,Int) Action GWRandomPolicy where
  mc_action pr s p =
    runRand $ uniform [minBound .. maxBound]


instance (Fractional num, Ord num, Show num) => MC_Policy_Show num GW (Int,Int) Action GWRandomPolicy
instance (Fractional num, Ord num, Show num) => MC_Problem_Show num GW (Int,Int) Action

gw :: GW Rational
gw = GW (4,4) (Set.fromList [(0,0),(3,3)])

gw_d :: GW Double
gw_d = GW (4,4) (Set.fromList [(0,0),(3,3)])

gw2 :: GW num
gw2 = GW (2,1) (Set.fromList [(1,0)])

forkThread :: IO () -> IO (MVar ())
forkThread proc = do
    handle <- newEmptyMVar
    _ <- forkFinally proc (\_ -> putMVar handle ())
    return handle

{- Uniform random policy evaluation using MC method -}
example_4_1_mc :: (Show num, Fractional num, Ord num, Real num) => GW num -> IO ()
example_4_1_mc gw = do
  let max = 200000
  let g = pureMT 42

  d1 <- newData "mc1"
  d2 <- newData "mc2"

  withPlot "plot1" [heredoc|
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
  |] $ do

  v_dp <- example_4_1_dp gw

  {- DP-to-MC Adapter -}
  {-
  t1 <- forkThread $
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
  -}

  {- Native MC implementation -}
  t2 <- forkThread $
    let
      opts = MC.defaultEvalOpts{
               o_max_iter = max,
               o_ext = E_Ext {
                 eo_learnMonitor = Just Monitor{
                   mon_target = v_dp,
                   mon_data = d2
                 }
               }
             }
    in do
    (v,_) <- MC.policy_eval opts gw GWRandomPolicy g
    showStateVal gw v

  mapM_ takeMVar [t2]



{- Uniform random policy evaluation using MC method -}
example_4_1_iter :: (Show num, Fractional num, Ord num, Real num) => GW num -> IO ()
example_4_1_iter gw = do
  let max = 200000
  let g = pureMT 42

  d <- newData "mc"

  withPlot "plot1" [heredoc|
    set grid back ls 102
    set xrange [0:${show max}]
    set yrange [-20:20]
    set terminal x11 1 noraise
    done = 0
    bind all 'd' 'done = 1'
    while(!done) {
      plot ${dat d} using 1:2 with lines
      pause 1
    }
  |] $
    let
      opts = MC.ES.defaultOpts{
               o_max_iter = max,
               o_ext = ES_Ext {
                 eo_debug = \ES_State{..} -> do
                  when (0 == _ess_iter `mod` 3000) $ do
                    showStateVal gw (q2v _ess_q)
                    showGenericPolicy gw _ess_p
               }
             }
      p = emptyGenericPolicy
      q = emptyQ
    in do
    (q,p) <- MC.ES.policy_iteraton opts gw (q,p) g
    return ()



