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
          if pocket <= 0 then
            0.0
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
  flip map (Map.toAscList gp_actions) $ \(Gambler{..}, set) ->
    let
      actmap = List.reverse $ List.sortOn fst $ Set.toList set
      (mx, Bet{..})
        | null actmap = (0, Bet 0)
        | otherwise = head $ actmap
    in
    (printf ("%02d: ") g_pocket) ++ (replicate bet_amount '#') ++ show bet_amount ++ "  " ++ show actmap

showStateVal StateVal{..} =
  unlines $
  flip map (Map.toAscList $ v_map) $ \ (Gambler{..},v) ->
    (printf ("%02d: ") g_pocket) ++ show v

debugState :: (StateVal Gambler, GenericPolicy Gambler Bet) -> IO ()
debugState (v,p) = do
  putStrLn $ showStateVal v
  putStrLn $ showPolicy p

example_gambler :: IO ()
example_gambler =
  let
      thegame = Game 100
      opts = defaultOpts {
          eo_max_iter=3
        , eo_gamma = 0.9
        , eo_etha = 0.001
        , eo_floating_precision = 1/(10^5)
        , eo_debug = debugState
      }
  in do
  (v,p) <-
    policy_iteraton thegame (uniformGenericPolicy thegame) (zero_sate_values thegame) opts

  -- putStrLn $ show $ policy_action_value thegame (Gambler 33) (Bet 33) opts v
  -- putStrLn $ show $ policy_action_value thegame (Gambler 33) (Bet 17) opts v
  putStrLn $ show $ policy_action_value thegame (Gambler 47) (Bet 47) opts v
  putStrLn $ show $ policy_action_value thegame (Gambler 47) (Bet 28) opts v
  putStrLn $ show $ policy_action_value thegame (Gambler 47) (Bet 22) opts v

