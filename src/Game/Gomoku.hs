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
import BitBoard        ( BitBoard(..)
                       , Size(..)
                       , hasNInRow
                       )
import Data.Bits       ((.|.), setBit, testBit)
import Data.List       (intersperse)
import Data.List.Split (chunksOf)

-- TODO: support more opening rules:
-- http://gomokuworld.com/gomoku/2

data Gomoku
    = Gomoku
    { black   :: BitBoard Integer
    , white   :: BitBoard Integer
    , current :: Player Gomoku
    } deriving (Eq, Show)

-- |According to Gomoku rules, the board size is fixed at 15x15
boardDim :: Int
boardDim = 15

boardSize :: Size
boardSize = Size (boardDim, boardDim)

numPositions = boardDim * boardDim

showBoard :: Gomoku -> String
showBoard s = (unlines . map (intersperse ' ') . chunksOf mx . reverse) flat
    where
        isBlack = testBit (black s)
        isWhite = testBit (white s)
        flat = [ char
               | i <- [0..numPositions-1]
               , let char
                       | isBlack i = 'X'
                       | isWhite i = 'O'
                       | otherwise     = '.' ]
        Size (mx, _) = boardSize

printBoard :: Gomoku -> IO ()
printBoard = putStr . showBoard

-- |A combined bitmask of all occupied positions
occupied :: Gomoku -> BitBoard Integer
occupied Gomoku { black = bs, white = ws }
    = bs .|. ws

-- |Lists all moves that haven't been played yet
unplaced :: Gomoku -> [Action Gomoku]
unplaced s
    = [ Place i
      | i <- [0..numPositions-1]
      , not $ testBit os i ]
    where os = occupied s

hasWin :: BitBoard Integer -> Bool
hasWin m = hasNInRow m 5

noMoves :: Gomoku -> Bool
noMoves = null . unplaced

instance State Gomoku where
    data Player Gomoku
        = Black
        | White
        deriving (Show, Eq)

    data Action Gomoku
        = Place Int
        deriving (Eq, Show)

    start = Gomoku { black   = BitBoard (0, boardSize)
                   , white   = BitBoard (0, boardSize)
                   , current = Black }

    currentTurn = current

    nextState s@Gomoku { current = p } (Place c)
        = case p of
            Black -> s { black = setBit (black s) c , current = White }
            White -> s { white = setBit (white s) c , current = Black }

    legalActions [] = map Place [0..numPositions-1]
    legalActions (s:_) = unplaced s

    winner [] = InProgress
    winner (s@Gomoku { black = bs, white = ws }:_)
        | hasWin bs = Win Black
        | hasWin ws = Win White
        | noMoves s = Draw
        | otherwise = InProgress
