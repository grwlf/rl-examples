{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Ch_4_Gambler where

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
import Debug.Trace

import DP

data Bet = Bet {
    bet_amount :: Int
  }
  deriving(Show, Eq, Ord)

data Game = Game {
    game_win_score :: Int
  }
  deriving(Show)

data Gambler = Gambler {
    g_pocket :: Int
  }
  deriving(Show, Eq, Ord)

instance RLProblem Game Gambler Bet where

  rl_states Game{..} =
    Set.fromList [Gambler pocket | pocket <- [0..game_win_score]]

  rl_actions Game{..} Gambler{..} =
    if g_pocket >= game_win_score || g_pocket <= 0 then
      Set.empty
    else
      let
        border = g_pocket `min` (game_win_score - g_pocket)
      in
      Set.fromList [Bet x | x <- [1..border]]

  rl_transitions Game{..} Gambler{..} Bet{..} =
    let

      reward pocket =
        if pocket >= game_win_score then
          1
        else
          if pocket <= 0 then
            0
          else
            0

      assign pocket' = (reward pocket', Gambler (game_win_score `min` pocket'))

    in
    if g_pocket >= game_win_score || g_pocket <= 0 then
        Set.empty
    else
        Set.fromList [
            (6%10, assign (g_pocket - bet_amount))
          , (4%10, assign (g_pocket + bet_amount))]

example_gambler :: IO ()
example_gambler =
  let
      thegame = Game 100
      opts = defaultOpts {
          eo_max_iter = 1
        , eo_gamma = 1.0
        , eo_etha = 0.00001
        , eo_debug = const $ return ()
      }

      showValPolicy (v@StateVal{..}, GenericPolicy{..}) =
        unlines $
        flip map (Map.toAscList v_map `zip` Map.toAscList gp_actions) $ \((_, vs) , (s@Gambler{..}, set)) ->
          let
            actmap = List.sortOn (\(p,a)-> a) $ Set.toList set

            show_actmap =
              List.intercalate "," $
              flip map actmap $ \(p,a@Bet{..}) -> show bet_amount ++ " (" ++ (printf "%2.5f" (stateval v s a)) ++ ")"

            (mx, Bet{..})
              | null actmap = (0, Bet 0)
              | otherwise = head $ actmap
            show_vs = printf "% 2.5f" (fromRational vs :: Double)
          in
          (printf ("%02d: ") g_pocket) ++ (replicate bet_amount '#') ++ show bet_amount ++ " " ++ show_vs ++ "  " ++ show_actmap

      debugState :: (StateVal Gambler, GenericPolicy Gambler Bet) -> IO ()
      debugState = putStrLn . showValPolicy

      stateval v s a = (fromRational $ policy_action_value thegame s a opts v :: Double)

  in do
  (v,p) <- policy_iteraton thegame (uniformGenericPolicy thegame) (zero_sate_values thegame) opts
  debugState (v,p)

