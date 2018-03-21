{-
  Simple Breakout-like game using Gloss Library
  João Rebelo Pires, 2018
-}

module Main where

import Graphics.Gloss

import Breakout.AtomicDefinitions
import Breakout.Renderer
import Breakout.Game
import Breakout.Keys

-- | main entry point
main :: IO ()
main = do
  play window black fps firstLevel render react update

window :: Display
window = InWindow "Breakout" (2*round maxWidth,2*round maxHeight) (0,0)
