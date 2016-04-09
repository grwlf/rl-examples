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
      Set.fromList [Bet x | x <- [1..g_pocket]]

  rl_transitions Game{..} Gambler{..} Bet{..} =
    let

      reward pocket =
        if pocket >= game_win_score then
          1.0
        else
          0.0

      assign income =
        let
            pocket' = g_pocket + income
        in
        (reward pocket', Gambler (game_win_score `min` pocket'))

    in
    if (abs (reward g_pocket)) < 1.0 then
        Set.fromList [
            (1%2, assign (0 - bet_amount))
          , (1%2, assign (0 + bet_amount))]
    else
        Set.empty

showPolicy :: GenericPolicy Gambler Bet -> String
showPolicy GenericPolicy{..} =
  unlines $
  map (\(Gambler{..}, set) ->
    let
      actmap = List.reverse $ List.sortOn fst $ Set.toList set
      (mx, Bet{..})
        | null actmap = (0, Bet 0)
        | otherwise = head $ actmap
    in
    printf ("%02d: " ++ (replicate bet_amount '#') ++ show bet_amount) g_pocket)
    (Map.toAscList gp_actions)

example_gambler :: IO ()
example_gambler =
  let
      thegame = Game 40
      opts = defaultOpts {
          eo_max_iter=2
        , eo_gamma = 0.9
        , eo_etha = 0.001
        , eo_debug = putStrLn . showPolicy
      }
  in do
  popt <-
    policy_iteraton thegame (uniformGenericPolicy thegame) (zero_sate_values thegame) opts
  return ()

