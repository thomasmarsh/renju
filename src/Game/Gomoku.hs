{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Game.Gomoku
    ( Gomoku(..)
    , Action(..)
    , start
    , currentTurn
    , nextState
    , legalActions
    , winner
    ) where

import State           ( State(..)
                       , Winner(..)
                       )
import BitBoard        ( BitBoard(..)
                       , hasNInRow
                       )
import Data.Bits       ((.|.), setBit, testBit)
import Data.List       (intersperse)
import Data.List.Split (chunksOf)

-- TODO: support more opening rules:
-- http://gomokuworld.com/gomoku/2


width :: Int
width = 15

numPositions :: Int
numPositions = width * width

type GomokuBoard = BitBoard 15 15 Integer

data Gomoku
    = Gomoku
    { black   :: GomokuBoard
    , white   :: GomokuBoard
    , current :: Player Gomoku
    } deriving Eq


instance Show Gomoku where
    show s = ( unlines
             . map (intersperse ' ')
             . chunksOf width . reverse
             ) flat
        where
            isBlack = testBit (black s)
            isWhite = testBit (white s)
            flat = [ char
                   | i <- [0..numPositions-1]
                   , let char
                           | isBlack i = 'X'
                           | isWhite i = 'O'
                           | otherwise = '.' ]

-- |A combined bitmask of all occupied positions
occupied :: Gomoku -> GomokuBoard
occupied Gomoku { black = bs, white = ws }
    = bs .|. ws

-- |Lists all moves that haven't been played yet
unplaced :: Gomoku -> [Action Gomoku]
unplaced s
    = [ Place i
      | i <- [0..numPositions-1]
      , not $ testBit os i ]
    where os = occupied s

hasWin :: GomokuBoard -> Bool
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

    start = Gomoku { black   = BitBoard 0
                   , white   = BitBoard 0
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
