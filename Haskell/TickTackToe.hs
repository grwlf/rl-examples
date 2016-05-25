{-# LANGUAGE MultiWayIf #-}
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

nextPlayer X = O
nextPlayer O = X
nextPlayer E = error "nextPlayer: result for E is undefined"

data Board = Board {
    bo_cells :: Map (Int,Int) Cell
  , bo_wins :: Player
} deriving(Show,Ord,Eq)

bo_term Board{..} = bo_wins == X || bo_wins == O

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

move :: Board -> Player -> Action -> Board
move Board{..} p a =
  case bo_wins of
    E ->
      let
        bo_cells' = Map.insert a p bo_cells
        bo_term' = or $ flip map (board_wincheck ! a) $ \r ->
                      all (==p) (map (\ a -> fromMaybe E $ Map.lookup a bo_cells') r)
      in
      Board bo_cells' (if bo_term' then p else E)
    _ -> error $ "move called on terminal board"

emptyBoard :: Board
emptyBoard = Board Map.empty E

boardFree :: Board -> [Action]
boardFree Board{..} = board_points \\ (Map.keys bo_cells)

-- isTerminal :: Board -> Cell -> Action -> Bool
-- isTerminal b c a =
--   let
--     b' = set a c b
--   in
--   or $
--     flip map (board_wincheck ! a) $ \r ->
--       all (==c) (map (b'`at`) r)

-- Same as boardFree + non-terminal
-- boardFreeSafe :: Player -> Board -> [Action]
-- boardFreeSafe c b = filter (not . (isTerminal b c)) (boardFree b)

-- FIXME: eliminate potantional forever loop
randomBoard :: (RandomGen g) => Player -> g -> (Board, g)
randomBoard p g =
  let
    check ((_,b,True),g') = (b,g')
    check ((_,_,False),g') =
      trace "randomBoard diverged at " $ randomBoard p g'
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
        board <- use _2
        let b's = filter ((==E) . bo_wins) $ map (move board player) (boardFree board)
        case null b's of
          True -> do
            _3 %= const False
            break ()
          False -> do
            b' <- Monad.uniform b's
            _1 %= nextPlayer
            _2 %= const b'
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
    g_vals :: Map Player (Q num Board Action)
  , g_player :: Player
} deriving(Show)

bestAction :: (Fractional num, Ord num, RandomGen g)
  => T num -> Board -> Player -> g -> Maybe (Action, g)
bestAction T{..} b p g =
  let
    macts = Map.lookup b $ view q_map $ g_vals ! p
  in
  case macts of
    Just as -> Just (fst $ maximumBy (compare `on` (current . snd)) (Map.toList as), g)
    Nothing ->
      case boardFree b of
        [] -> Nothing
        x -> Just $ flip runRnd g $ Monad.uniform x


instance (Fractional num, Ord num) => MC_Problem num T Board Action where

  mc_state_nonterm T{..} = randomBoard g_player

  mc_actions T{..} = Set.fromList . boardFree

  mc_transition t@T{..} b a g =
    let
      {- Applying action -}
      b' = move b g_player a
    in
    case bo_term b' of
      True ->
        {- Win -}
        ((b', True), g)
      False ->
        let
          p' = nextPlayer g_player
        in
        case bestAction t b' p' g of
          Nothing ->
            {- Draw -}
            ((b', True), g)
          Just (a',g') ->
            {- Make opponent's move according to their current state-value -}
            let
              b'' = move b' p' a'
            in
            {- Next|Loose -}
            ((b'', bo_term b''), g')

  mc_reward T{..} b a b' =
    if | bo_wins b' == g_player -> 1
       | otherwise -> 0

