{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lsystem where

import Control.Monad.Extra
import Control.Monad.State.Strict

data Alphabet = F | L | R | P | J deriving (Eq, Show)

rule :: Alphabet -> [Alphabet]
rule = \case
  F -> [F, L, F, R, F, L, F, L, F]
  L -> [L]
  R -> [R]
  P -> [L, F]
  J -> [J]

expand :: [a] -> (a -> [a]) -> [a]
expand seed productionRule = concatMap productionRule seed

generateMax :: [a] -> (a -> [a]) -> Int -> [a]
generateMax seed productionRule maxIterations
  | maxIterations <= 0 = seed
  | otherwise =
    let nextSeed = expand seed productionRule
    in generateMax nextSeed productionRule (maxIterations - 1)
