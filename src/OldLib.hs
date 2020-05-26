{-# LANGUAGE TupleSections #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OldLib where

--import Probability
import System.Environment
import Data.Char
import qualified Data.Map.Lazy as Map
import Control.Monad.State
import Data.Maybe

type Prob = Double
type Count = Double
data Token a = Single a | Double a a deriving (Eq, Ord)
type TokenCounts a = Map.Map (Token a) Count
type TokenProbs a = Map.Map (Token a) Prob
type Segment a = [a]

run :: String -> IO ()
run fileName = do
  text <- readFile fileName
  let contents = processText text
      singleCounts = count $ singles contents
      doubleCounts = count $ doubles contents
      singleProbs = asDist singleCounts
      singleInfos = Map.map infoContent singleProbs
  pprint singleCounts
  pprint singleProbs
  pprint singleInfos

instance (Show a) => Show (Token a) where
  show (Single x) = show x
  show (Double x y) = show (x, y)

{--
instance Show Prob where
  show (P p) = show intPart ++ "." ++ show fracPart ++ "%"
    where digits = round (10000 * p)
          intPart = digits `div` 100
          fracPart = digits `mod` 100
--}

processText :: String -> String
processText = map (replaceDigit . toLower) . filter isAlphaNum . filter isAscii

replaceDigit :: Char -> Char
replaceDigit x = if isDigit x then '#' else x

pprint :: (Show a, Show b) => Map.Map (Token a) b -> IO ()
pprint = mapM_ (putStrLn . \(k, v) -> show k ++ " " ++ show v) . Map.toList

count :: (Ord a) => [Token a] -> TokenCounts a
count = Map.fromListWith (+) . map (,1) 

singles :: String -> [Token Char]
singles = map Single

doubles :: String -> [Token Char]
doubles = snd . foldl (\(x, list) y -> (y, Double x y : list)) (' ', [])

asDist :: TokenCounts a -> TokenProbs a
asDist kvs = Map.map (/ total) kvs
  where total = sum $ Map.elems kvs

infoContent :: Double -> Double
infoContent = negate . logBase 2

given :: (Eq a) => a -> TokenProbs a -> [Double]
given x = Map.elems . Map.filterWithKey (\(Double y _) _ -> x == y)

entropy :: (Eq a) => a -> TokenProbs a -> Double
entropy x = sum . map (\p -> p * infoContent p) . given x



push :: a -> State (Segment a) ()
push a = state $ \xs -> ((), a:xs)

flush :: State (Segment a) (Segment a)
flush = state (, [])

{-
isBoundary :: (Eq a) => TokenProbs a -> a -> a -> Bool
isBoundary kvs x y = isJust $ do
  px <- lookup x kvs
  py <- lookup y kvs
  return $ (infoContent px) < (infoContent py)
-}