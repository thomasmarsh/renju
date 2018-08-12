{-# LANGUAGE TypeFamilies #-}

module Game.Gomoku
    ( Gomoku(..)
    , Action(..)
    , start
    , currentTurn
    , nextState
    , legalActions
    , winner
    , boardSize
    , showBoard
    , printBoard
    ) where

import State           ( State(..)
                       , Winner(..)
                       )
import BitBoard        ( Mask(..)
                       , Coord(..)
                       , Size(..)
                       , hasNInRow
                       , isSet
                       , set
                       )
import Data.Bits       ((.|.))
import Data.List       (intersperse)
import Data.List.Split (chunksOf)

-- TODO: support more opening rules:
-- http://gomokuworld.com/gomoku/2

data Gomoku
    = Gomoku
    { black   :: Mask Integer
    , white   :: Mask Integer
    , current :: Player Gomoku
    } deriving (Eq, Show)

-- |According to Gomoku rules, the board size is fixed at 15x15
boardDim :: Int
boardDim = 15

boardSize :: Size
boardSize = Size (boardDim, boardDim)

showBoard :: Gomoku -> String
showBoard s = (unlines . map (intersperse ' ') . chunksOf mx . reverse) flat
    where
        isBlack = isSet boardSize (black s)
        isWhite = isSet boardSize (white s)
        flat = [ char
               | coord <- allCoords
               , let char
                       | isBlack coord = 'X'
                       | isWhite coord = 'O'
                       | otherwise     = '.' ]
        Size (mx, _) = boardSize

printBoard :: Gomoku -> IO ()
printBoard = putStr . showBoard

-- |A Coord for every intersection on the board
allCoords :: [Coord]
allCoords =
    [ Coord (x,y)
    | y <- [0..my-1]
    , x <- [0..mx-1] ]
    where (Size (mx, my)) = boardSize

-- |A combined bitmask of all occupied positions
occupied :: Gomoku -> Mask Integer
occupied Gomoku { black = Mask bs, white = Mask ws }
    = Mask (bs .|. ws)

-- |Lists all moves that haven't been played yet
unplaced :: Gomoku -> [Action Gomoku]
unplaced s
    = [ Place coord
      | coord <- allCoords
      , not $ isSet boardSize os coord ]
    where os = occupied s

hasWin :: Mask Integer -> Bool
hasWin m = hasNInRow boardSize m 5

noMoves :: Gomoku -> Bool
noMoves = null . unplaced

instance State Gomoku where
    data Player Gomoku
        = Black
        | White
        deriving (Show, Eq)

    data Action Gomoku
        = Place Coord
        deriving (Eq, Show)

    start = Gomoku { black   = Mask 0
                   , white   = Mask 0
                   , current = Black }

    currentTurn = current

    nextState s@Gomoku { current = p } (Place c)
        = case p of
            Black -> s { black = set boardSize (black s) c , current = White }
            White -> s { white = set boardSize (white s) c , current = Black }

    legalActions [] = map Place allCoords
    legalActions (s:_) = unplaced s

    winner [] = InProgress
    winner (s@Gomoku { black = bs, white = ws }:_)
        | hasWin bs = Win Black
        | hasWin ws = Win White
        | noMoves s = Draw
        | otherwise = InProgress
