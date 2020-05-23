{-# LANGUAGE TupleSections #-} 

module Lib where

import System.Environment
import Data.Char
import qualified Data.Map.Lazy as Map

run :: String -> IO ()
run fileName = do
  text <- readFile fileName
  let 
    contents = processText text
    singleCounts = count $ singles contents
    doubleCounts = count $ doubles contents
    singleProbs = asDistribution singleCounts
  pprint doubleCounts

type Prob = Rational
type Count = Int
data Token a = Single a | Double a a deriving (Eq, Ord)
type TokenCounts a = Map.Map (Token a) Count
type TokenProbs a = Map.Map (Token a) Prob

instance (Show a) => Show (Token a) where
  show (Single x) = show x
  show (Double x y) = show (x, y)

processText :: String -> String
processText = map (replaceDigit . toLower) . filter isAlphaNum . filter isAscii

replaceDigit :: Char -> Char
replaceDigit x = if isDigit x then '#' else x

pprint :: (Show a, Show b) => Map.Map (Token a) b -> IO ()
pprint = mapM_ (putStrLn . \(k, v) -> show k ++ " " ++ show v) . Map.toList

count :: (Ord a) => [Token a] -> TokenCounts a
count = Map.fromListWith (+) . map (,1) 
--count = foldl increment Map.empty

singles :: String -> [Token Char]
singles = map Single

doubles :: String -> [Token Char]
doubles = snd . foldl (\(x, list) y -> (y, Double x y : list)) (' ', [])

asDistribution :: TokenCounts a -> TokenProbs a
asDistribution kvs = Map.map (\v -> fromIntegral v / total) kvs
  where total = fromIntegral $ sum $ Map.elems kvs

someFunc :: IO ()
someFunc = putStrLn "someFunc"
