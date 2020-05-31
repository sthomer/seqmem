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
data Level a = Level { unigram :: Unigram a, bigram :: Bigram a }

run :: String -> IO ()
run fileName = do
  text <- readFile fileName
  let contents0 = processText text
      -- unigram0 = asDist $ countUnigram contents0
      -- contents1 = flatten $ segmentUnigram unigram0 [] contents0
      -- unigram1 = asDist $ countUnigram contents1
      bigram0 = asDist $ countBigram contents0
      contents1 = flatten $ segmentBigram bigram0 [] contents0
      bigram1 = asDist $ countBigram contents1
      contents2 = flatten $ segmentBigram bigram1 [] contents1
  -- pprint unigram4
  -- pprint bigram1
  -- print $ take 100 contents0
  print $ take 100 contents2
  return ()

processText :: String -> [Token Char]
processText = map (tokenize . replaceDigit . toLower) . filter isAlphaNum .filter isAscii

replaceDigit :: Char -> Char
replaceDigit x = if isDigit x then '#' else x

tokenize :: a -> [a]
tokenize x = [x]

level :: Level a
level = Level { unigram = M.empty, bigram = M.empty }

pprint :: (Show a, Show b) => M.Map a b -> IO ()
pprint = mapM_ (putStrLn . \(k, v) -> show k ++ " " ++ show v) . M.toList

pairs :: [a] -> [(a, a)]
pairs xs@(_:ys) = zip xs ys

incUnigram :: (Ord a) => Token a -> Unigram a -> Unigram a
incUnigram x = M.insertWith (+) x 1

incBigram :: (Ord a) => (Token a, Token a) -> Bigram a -> Bigram a
incBigram x = M.insertWith (+) x 1

countUnigram :: (Ord a) => [Token a] -> Unigram a
countUnigram = foldr incUnigram M.empty

countBigram :: (Ord a) => [Token a] -> Bigram a
countBigram = foldr incBigram M.empty . pairs

given :: (Eq a) => Token a -> Bigram a -> Bigram a
given x = M.filterWithKey (\(z, _) _ -> x == z)

asDist :: M.Map a Double -> M.Map a Double
asDist kvs = M.map (/ total) kvs where total = sum $ M.elems kvs

infoContent :: Double -> Double
infoContent = negate . logBase 2

entropy :: Bigram a -> Double
entropy = sum . M.map (\p -> p * infoContent p)

isMoreUnexpected :: (Ord a) => Unigram a -> Token a -> Token a -> Bool
isMoreUnexpected kvs x y = (kvs M.! x) < (kvs M.! y)

isMoreUncertain :: (Ord a) => Bigram a -> Token a -> Token a -> Bool
isMoreUncertain kvs x y = entropy (given x kvs) < entropy (given y kvs)

segmentUnigram :: (Ord a) => Unigram a -> [Token a] -> [Token a] -> [[Token a]]
segmentUnigram kvs s [] = [s]
segmentUnigram kvs s [_] = [s]
segmentUnigram kvs s (x:y:xs) = if isMoreUnexpected kvs x y 
  then reverse (x:s) : segmentUnigram kvs [] (y:xs)
  else segmentUnigram kvs (x:s) (y:xs)

-- TODO: Too slow => Precompute all entropies
segmentBigram :: (Ord a) => Bigram a -> [Token a] -> [Token a] -> [[Token a]]
segmentBigram kvs s [] = [s]
segmentBigram kvs s [_] = [s]
segmentBigram kvs s (x:y:xs) = if isMoreUncertain kvs x y
  then reverse (x:s) : segmentBigram kvs [] (y:xs)
  else segmentBigram kvs (x:s) (y:xs)

flatten :: [[Token a]] -> [Token a]
flatten = map concat