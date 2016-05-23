module TickTackToe where

import Imports

import MC.ES

data XO = X | O
  deriving(Show,Eq,Ord)

data Board = {
  board_cells :: Map (Int,Int) XO
} deriving(Show)

data G = G {
}
deriving(Show)


instance (Fractional num, Ord num) => DP_Problem num G Board Move where
  rl_states G = Set.fromList [(x,y) | x <- [0..sx-1], y <- [0..sy-1]]

  -- rl_actions pr s =
  --   case Set.member s (rl_terminal_states pr) of
  --     True -> Set.empty
  --     False -> Set.fromList [minBound..maxBound]

  -- rl_transitions (GW (sx,sy) _) (x,y) a =
  --   let
  --     check (x',y') =
  --       if x' >= 0 && x' < sx && y' >= 0 && y' < sy then
  --         (x',y')
  --       else
  --         (x,y)
  --   in
  --   Set.fromList [(
  --       case a of
  --          L -> check (x-1,y)
  --          R -> check (x+1,y)
  --          U -> check (x,y-1)
  --          D -> check (x,y+1)
  --       , 1%1)]

  -- rl_reward (GW (sx,sy) _) s a s' = -1

  -- rl_terminal_states (GW _ exits) = exits
