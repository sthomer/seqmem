{-# LANGUAGE TupleSections #-}

module Lib where

import System.Environment
import Data.Char
import Control.Monad.State
import qualified Data.Map.Lazy as M
import Data.Maybe

type Token a = [a]
type Segment a = [Token a]
type Unigram a = M.Map (Token a) Double
type Bigram a = M.Map (Token a, Token a) Double
data Level a = Level { unigram :: Unigram a, bigram :: Bigram a, 
                       segment :: Segment a, prev :: Maybe (Token a) }

run :: String -> IO ()
run fileName = do
  text <- readFile fileName
  let contents = processText text
  return ()

processText :: String -> String
processText = map (replaceDigit . toLower) . filter isAlphaNum .filter isAscii

replaceDigit :: Char -> Char
replaceDigit x = if isDigit x then '#' else x

level :: Level a
level = Level { unigram = M.empty, bigram = M.empty, segment = [], prev = Nothing }

clearSegment :: Level a -> Level a
clearSegment l = l { segment = [] }

incPrevious :: Level a -> Token a -> Level a
incPrevious l x = l { prev = Just x }

incSegment :: Level a -> Token a -> Level a
incSegment l @ Level { segment = xs } x = l { segment = x:xs }

incUnigram :: (Ord a) => Level a -> Token a -> Level a
incUnigram l @ Level { unigram = kvs } x = l { unigram = M.insertWith (+) x 1 kvs}

incBigram :: (Ord a) => Level a -> (Token a, Token a) -> Level a
incBigram l @ Level { bigram = kvs } x = l { bigram = M.insertWith (+) x 1 kvs }

pushSegment :: Token a -> State (Level a) ()
pushSegment x = state $ \l -> ((), incSegment l x)

pushUnigram :: (Ord a) => Token a -> State (Level a) ()
pushUnigram x = state $ \l -> ((), incUnigram l x)

pushBigram :: (Ord a) => Token a -> State (Level a) ()
pushBigram x = state $ \l @ Level { prev = Just y } -> ((), incBigram l (x, y))

push :: (Ord a) => Token a -> State (Level a) ()
push x = do 
  pushBigram x
  pushUnigram x
  pushSegment x

flush :: State (Level a) (Segment a)
flush = state $ \l -> (segment l, clearSegment l)

isMoreUnexpected :: (Ord a) => Token a -> Token a -> Unigram a -> Bool
isMoreUnexpected x y kvs = (kvs M.! x) < (kvs M.! y)

isMoreUncertain :: (Ord a) => Token a -> Token a -> Bigram a -> Bool
isMoreUncertain x y kvs = entropy (given x kvs) < entropy (given y kvs)

given :: (Eq a) => Token a -> Bigram a -> Bigram a
given x = M.filterWithKey (\(z, _) _ -> x == z)

asDist :: Bigram a -> Bigram a
asDist kvs = M.map (/ total) kvs where total = sum $ M.elems kvs

infoContent :: Double -> Double
infoContent = negate . logBase 2

entropy :: Bigram a -> Double
entropy = sum . M.map (\p -> p * infoContent p)

