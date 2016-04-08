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
    Set.fromList [Gambler pocket | pocket <- [0..game_win_score-1]]

  rl_actions Game{..} Gambler{..} = Set.fromList [Bet x | x <- [1..g_pocket]]

  rl_transitions Game{..} Gambler{..} Bet{..} =
    let

      reward pocket =
        if pocket >= game_win_score then
          1.0
        else
          if pocket <= 0 then
            -1.0
          else
            0.0

      assign income =
        let
            pocket' = g_pocket + income
        in
        (reward pocket', Gambler pocket')

    in
    if (abs (reward g_pocket)) < 1.0 then
        Set.fromList [
            (1%2, assign (0 - bet_amount))
          , (1%2, assign (0 + bet_amount))]
    else
        Set.empty

example_gambler :: IO ()
example_gambler =
  let
      thegame = Game 10
      opts = defaultOpts{eo_max_iter=5, eo_gamma = 1, eo_etha = 0.001}
  in do
  popt <- policy_iteraton thegame (uniformGenericPolicy thegame) (zero_sate_values thegame) opts
  return ()

