{-# LANGUAGE LambdaCase #-}

module Main where

import Graphics.WorldTurtle

import Control.Monad
import Lsystem

drawLSystem :: [Alphabet] -> TurtleCommand ()
drawLSystem seed = do
  forM_ seed $ \sym -> do
    drawSym sym
  where
    drawSym :: Alphabet -> TurtleCommand ()
    drawSym = \case
      F -> forward 5
      L -> left 60
      R -> right 60

main :: IO ()
main = do
  let seed = generateMax [F, L, F, L, F, L, F] rule 5
  runTurtle $ drawLSystem seed
