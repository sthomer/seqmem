{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Probability where

import System.Random

newtype Prob = Prob { toDouble :: Double } 
  deriving (Show, Eq, Ord, Num, Fractional, 
            Real, RealFrac, Floating, Random) 

prob :: Double -> Prob
prob = Prob