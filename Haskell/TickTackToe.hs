{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module TickTackToe where

import Imports
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Monad
import Types
import MC.ES

data Cell = X | O | E
  deriving(Show,Eq,Ord)

nextCell X = O
nextCell O = X
nextCell E = error "nextCell E is undefined"

data Board = Board {
  bo_cells :: Map (Int,Int) Cell
} deriving(Show)

type Action = (Int,Int)

board_ny = 3
board_nx = 3
(.+) (a,b) (c,d) = (a+c,b+d)
(.*) x (a,b) = (x*a,x*b)
board_inside (x,y) = x >=0 && x < board_nx && y >=0 && y < board_ny
board_winsize = 3
board_points = [(x,y) | x<-[0..board_nx-1], y<-[0..board_ny-1]]
board_dirs = [(0,1),(1,0),(1,1),(1,-1)]
board_rows = filter ( and . map board_inside ) $ [ map (\x -> p .+ (x .* d)) [0..board_winsize-1] | p <- board_points, d <- board_dirs]
board_wincheck = Map.fromList [ (p,filter (p`elem`) board_rows) | p <- board_points ]

at :: Board -> Action -> Cell
at Board{..} a = fromMaybe E (Map.lookup a bo_cells)

set a c Board{..} = Board (Map.insert a c bo_cells)

emptyBoard :: Board
emptyBoard = Board Map.empty

boardFree :: Board -> [Action]
boardFree Board{..} = board_points List.\\ (Map.keys bo_cells)

isTerminal :: Board -> Cell -> Action -> Bool
isTerminal b c a =
  let
    b' = set a c b
  in
  or $
    flip map (board_wincheck ! a) $ \r ->
      all (==c) (map (b'`at`) r)

-- Same as boardFree + non-terminal
boardFreeSafe :: Cell -> Board -> [Action]
boardFreeSafe c b = filter (not . (isTerminal b c)) (boardFree b)

randomBoard :: PureMT -> (Board, PureMT)
randomBoard g = do
  flip runRnd g $ do
  snd <$> do
  flip execStateT (X, emptyBoard) $ do
  nmoves <- getRndR (0,board_nx*board_ny-1)
  forM_ [0..nmoves] $ \m -> do
    player <- use _1
    avail <- uses _2 (boardFreeSafe player)
    move <- Monad.uniform avail
    _1 %= nextCell
    _2 %= set move player

-- randomBoard :: PureMT -> (Board, PureMT)
-- randomBoard g =
--   let
--     (w,g') = randomWord64 g
--   in
--   (Board $
--   Map.fromList $ concat $
--   flip map board_points $ \(x,y) ->
--     let
--       pos = y*board_nx + x
--     in
--     if testBit w (2*pos) then
--         if testBit w (2*pos+1) then [((x,y),X)] else [((x,y),O)]
--     else []
--   ,g')

showBoard :: Board -> IO ()
showBoard b =
  forM_ [0..board_ny-1] $ \y -> do
    forM_ [0..board_nx-1] $ \x -> do
      putStr $ show (b `at` (x,y))
    putStrLn ""

data G = G {
} deriving(Show)


-- instance (Fractional num, Ord num) => DP_Problem num G Board Move where
--   rl_states G = Set.fromList [(x,y) | x <- [0..sx-1], y <- [0..sy-1]]

--   rl_actions pr s =
--     case Set.member s (rl_terminal_states pr) of
--       True -> Set.empty
--       False -> Set.fromList [minBound..maxBound]

--   rl_transitions (GW (sx,sy) _) (x,y) a =
--     let
--       check (x',y') =
--         if x' >= 0 && x' < sx && y' >= 0 && y' < sy then
--           (x',y')
--         else
--           (x,y)
--     in
--     Set.fromList [(
--         case a of
--            L -> check (x-1,y)
--            R -> check (x+1,y)
--            U -> check (x,y-1)
--            D -> check (x,y+1)
--         , 1%1)]

--   rl_reward (GW (sx,sy) _) s a s' = -1

--   rl_terminal_states (GW _ exits) = exits
