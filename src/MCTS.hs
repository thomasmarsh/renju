{-# LANGUAGE DatatypeContexts    #-} -- TODO: remove
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module MCTS where

import State (State(..), Winner(..))

import Control.Arrow   ((&&&), first)
import Data.List       (maximumBy, partition)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Random   (RandomGen, randomR)

data MCT a
    = MCT
    { rState     :: a
    , rPlayed    :: Int
    , rQ         :: [(Player a, Double)]
    , rChildList :: [MCT a]
    }

type ScoreTuple a = [(a, Score)]
type Score        = Double
type Plays        = Int

data (State a, RandomGen g) => Config a g
    = Config
    { expandConst :: Int
    , select :: (State a, RandomGen g) => [(ScoreTuple (Player a), Plays)] -> Player a -> g -> (Int, g)
    , backProp :: State a => Winner (Player a)
                          -> (ScoreTuple (Player a), Plays)
                          -> (ScoreTuple (Player a), Plays)
    }

defaultConfig :: (State a, RandomGen g) => Config a g
defaultConfig
    = Config
    { expandConst = 1
    , select = ucb 1
    , backProp =
        \s -> case s of
                InProgress -> id
                Draw       -> \(a,b) -> (a, b+1)
                Win p      -> \(a,b) -> (update (==p) (+1) (+(-1)) a, b+1)
    }


update :: (a -> Bool) -> (Score -> Score) -> (Score -> Score) -> ScoreTuple a -> ScoreTuple a
update b f g = map (\(p,q) -> if b p
                              then (p, f q)
                              else (p, g q))

ucb :: (State a, RandomGen g)
    => Double -> [(ScoreTuple (Player a), Plays)] -> Player a -> g -> (Int, g)
ucb c list p g
    | null list                             = error "can't perform ucb from an empty list"
    | null played                           = (snd randomUnplayed, g')
    | ansscore > csqrtlogN || null unplayed = (ans, g)
    | otherwise                             = (snd randomUnplayed, g')
    where
        (unplayed, played)   = partition ((==0) . snd . fst) $ zip list [0..]
        (randomUnplayed, g') = pick g unplayed
        (ansscore, ans)      = maximum $ map (first ucbScore) played
        csqrtlogN            = (*c) . sqrt . log . fromIntegral . sum $ map snd list
        ucbScore (st, n)     = averageScore (st, n) + (csqrtlogN / fromIntegral (n+1))
        averageScore (_, 0)  = 0
        averageScore (st, n) = readTuple p st / fromIntegral n

selectBestMove :: MCT a -> a
selectBestMove t
    = rState
    $ maximumBy (\a b -> compare (rPlayed a) (rPlayed b))
    $ rChildList t

toLeaf :: State a => a -> MCT a
toLeaf s = MCT
         { rState     = s
         , rPlayed    = 0
         , rQ         = map (\x -> (x,0)) allPlayers
         , rChildList = []
         }

expand :: State a => MCT a -> MCT a
expand t = t { rChildList = map (expand . toLeaf)
                          . legalChildren
                          $ rState t }

pick :: RandomGen g => g-> [a] -> (a, g)
pick _ [] = error "picking from empty list"
pick gen xs =
    let
        (number, gen') = randomR (0,length xs-1) gen
    in (xs!!number, gen')

readTuple :: Eq p => p -> ScoreTuple p -> Score
readTuple _ [] = error "tried to read a list with non-element"
readTuple p ((p',n'):ps) = if p == p' then n' else readTuple p ps

mcts :: (State a, RandomGen g) => MCT a -> Config a g -> g -> (MCT a, Winner (Player a), g)
mcts t conf g
    | s /= InProgress = (backP t s, s, g)
    | 0 == rPlayed t  = (backP t ssim, ssim, g')
    | otherwise       = (backP t { rChildList = st':sts } ssel, ssel, g''')
    where
        (ssim, g') = rollout (rState t) g

        selectionParams = map (rQ &&& rPlayed) $ rChildList t
        (selectionIndex, g'') = select conf selectionParams p g
        (st, sts) = rChildList t ~!!~ selectionIndex
        (st', ssel, g''') = mcts st conf g''

        s = winner [rState t]
        p = currentTurn $ rState t

        backP x state = x { rQ = bigQ', rPlayed = n' }
            where
                (bigQ',n') = backProp conf state (rQ x, rPlayed x)

(~!!~) :: [a] -> Int-> (a,[a])
[]     ~!!~ _ = error "empty list"
(x:xs) ~!!~ 0 = (x,xs)
(x:xs) ~!!~ n = (y, x:ys)
    where (y,ys) = xs ~!!~ (n-1)


{-|
  Do a number of iterations of /MCTS/ and return the most desirable game position.
  For example:

  @
  let (myGame',myRandomSeed') = doIterativeMcts myGame 'defaultConfig' myRandomSeed n
  @

  where n is the number of iterations to do.
-}
doIterativeMcts :: (State a, RandomGen g) => a -> Config a g -> Int -> g -> (a,g)
doIterativeMcts game conf n gen = (selectBestMove endTree, gen')
  where
    startTree = expand $ toLeaf game
    (endTree, gen') = f startTree n gen
--    f :: MCT a -> Int -> g
    f t 0 g = (t, g)
    f t n' g = let (t', _, g') = mcts t conf g in f t' (n'-1) g'
{-|
  Do as many possible iterations of /MCTS/ possible in the availiable time and return
  the most desirable game position wrapped up in IO monad. Most common use would be
  directly in the main method, for example:

  @
  main = do
           ...
           (myGame',myRandomSeed') <- doTimedMcts myGame 'defaultConfig' myRandomSeed n
           ...
  @

  where @n@ is the number of milliseconds to search for. Note, the search will take
  a few milliseconds longer than this value, so you will have to play with this value
  if you are under strict time constraints.
-}

doTimedMcts :: (State a, RandomGen g) => a -> Config a g -> g -> Int -> IO (a,g)
doTimedMcts game conf gen n = do
    currentTimestamp <- getCurrentTime
    let maxTimeStamp = addUTCTime (fromIntegral n / 1000) currentTimestamp
    let startTree = expand $ toLeaf game
    (endTree, gen') <- f startTree maxTimeStamp 0 gen
    return (selectBestMove endTree, gen')
    where
      f t xn n' g = do
                  currentTimestamp<-getCurrentTime
                  if currentTimestamp < xn then do
                    let (t', _, g') = mcts t conf g
                    (((f $! t') $! xn) $! (n'+1 :: Integer)) $! g'
                  else do
                    print n'
                    return (t, g)
