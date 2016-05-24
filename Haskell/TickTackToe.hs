{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module TickTackToe where

import Imports
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding(break)

import Monad
import Types
import MC.ES
import MC.Types

data Cell = X | O | E
  deriving(Show,Eq,Ord)

type Player = Cell

nextCell X = O
nextCell O = X
nextCell E = error "nextCell E is undefined"

data Board = Board {
  bo_cells :: Map (Int,Int) Cell
} deriving(Show,Ord,Eq)

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
boardFreeSafe :: Player -> Board -> [Action]
boardFreeSafe c b = filter (not . (isTerminal b c)) (boardFree b)

randomBoard :: (RandomGen g) => Player -> g -> (Board, g)
randomBoard p g =
  let
    check ((_,b,True),g') = (b,g')
    check ((_,_,False),g') = randomBoard p g'
  in do
  check $
    flip runRnd g $ do
    flip execStateT (X, emptyBoard, True) $ do
    loop $ do
      nmoves <- (
        case p of
          X -> Monad.uniform $ filter odd [0 .. board_nx*board_ny-1]
          O -> Monad.uniform $ filter even [0 .. board_nx*board_ny-1])
      forM_ [0..nmoves] $ \m -> do
        player <- use _1
        avail <- uses _2 (boardFreeSafe player)
        case null avail of
          True -> do
            _3 %= const False
            break ()
          False -> do
            move <- Monad.uniform avail
            _1 %= nextCell
            _2 %= set move player
      break ()

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


-- | TickTackToe => T
data T num = T {
    g_oppoment_val :: StateVal num Board
  , g_opponent :: Cell
} deriving(Show)


instance (Fractional num, Ord num) => MC_Problem num T Board Action where

  mc_state_nonterm T{..} = randomBoard g_opponent

  mc_actions T{..} = Set.fromList . boardFree

  mc_transition T{..} b a g =
    {- Applying action -}
    {- Check win|loose -}
    {- Make opponent's move according to its current state-value -}
    undefined

  mc_reward T{..} b a b' =
    {- 1 if win -}
    {- 0 otherwise -}
    undefined

