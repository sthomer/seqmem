{-# LANGUAGE TupleSections #-}

module Lib where

import System.Environment
import Data.Char
import Control.Monad.State
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.List

type Token a = [a]
type Segment a = [Token a]
type SingleCounts a = M.Map (Token a) Int
type DoubleCounts a = M.Map (Token a, Token a) Int
type Probabilities a = M.Map (Token a) Double
type Entropies a = M.Map (Token a) Double

run :: String -> IO ()
run fileName = do
  text <- readFile fileName
  let contents0 = processText text
      probs0 = probabilities contents0
      entropies0 = entropies contents0
      contents1 = segmentBoth probs0 entropies0 contents0
      probs1 = probabilities contents1
      entropies1 = entropies contents1
      contents2 = segmentBoth probs1 entropies1 contents1
      probs2 = probabilities contents2
      entropies2 = entropies contents2
      contents3 = segmentBoth probs2 entropies2 contents2
      probs3 = probabilities contents3
      entropies3 = entropies contents3
      contents4 = segmentBoth probs3 entropies3 contents3
  print $ take 100 contents3
  print $ take 100 contents4
  return ()

processText :: String -> [Token Char]
processText = map (tokenize . replaceDigit . toLower) . filter isAlphaNum .filter isAscii

replaceDigit :: Char -> Char
replaceDigit x = if isDigit x then '#' else x

tokenize :: a -> [a]
tokenize x = [x]

pprint :: (Show a, Show b) => M.Map a b -> IO ()
pprint = mapM_ (putStrLn . \(k, v) -> show k ++ " " ++ show v) . M.toList

pairs :: [a] -> [(a, a)]
pairs xs@(_:ys) = zip xs ys

flatten :: [[Token a]] -> [Token a]
flatten = map concat

count :: (Ord a) => [a] -> M.Map a Int
count = M.fromListWith (+) . map (,1)

given :: (Ord a) => Token a -> DoubleCounts a -> SingleCounts a
given x kvs = M.mapKeys snd (M.filterWithKey (\(z, _) _ -> x == z) kvs)

asDist :: M.Map a Int -> M.Map a Double
asDist kvs = M.map ((/ total) . fromIntegral) kvs 
  where total = fromIntegral $ sum $ M.elems kvs

infoContent :: Double -> Double
infoContent = negate . logBase 2

entropy :: Probabilities a -> Double
entropy = sum . M.map (\p -> p * infoContent p)

entropies :: (Eq a, Ord a) => [Token a] -> Entropies a
entropies xs = M.fromList $ map entropyFor keys
  where kvs = count $ pairs xs
        keys = nub $ map fst (M.keys kvs)
        entropyFor x = (x, entropy $ asDist $ given x kvs)

probabilities :: (Ord a) => [Token a] -> Probabilities a
probabilities = asDist . count

isMoreUnexpected :: (Ord a) => Probabilities a -> Token a -> Token a -> Bool
isMoreUnexpected kvs x y = fromMaybe False $ do 
  px <- M.lookup x kvs
  py <- M.lookup y kvs
  return $ px > py

isMoreUncertain :: (Ord a) => Entropies a -> Token a -> Token a -> Bool
isMoreUncertain kvs x y = fromMaybe False $ do 
  ex <- M.lookup x kvs
  ey <- M.lookup y kvs
  return $ ex < ey

segmentBy :: (Ord a) => (Token a -> Token a -> Bool) -> [Token a] -> [Token a] -> [[Token a]]
segmentBy p s [] = [s]
segmentBy p s [_] = [s]
segmentBy p s (x:y:xs)
  | p x y = reverse (x:s) : segmentBy p [] (y:xs)
  | otherwise = segmentBy p (x:s) (y:xs)

segmentInfoContent :: (Ord a) => Probabilities a -> [Token a] -> [Token a]
segmentInfoContent kvs xs = flatten $ segmentBy (isMoreUnexpected kvs) [] xs

segmentEntropy :: (Ord a) => Entropies a -> [Token a] -> [Token a]
segmentEntropy kvs xs = flatten $ segmentBy (isMoreUncertain kvs) [] xs

or2p :: (a -> a -> Bool) -> (a -> a -> Bool) -> (a -> a -> Bool)
(f `or2p` g) x y = f x y || g x y

segmentBoth :: (Ord a) => Probabilities a -> Entropies a -> [Token a] -> [Token a]
segmentBoth ps es xs = flatten $ segmentBy (isMoreUnexpected ps `or2p` isMoreUncertain es) [] xs