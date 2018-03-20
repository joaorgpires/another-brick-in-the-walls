{-
  Scoring module using Gloss Library
  João Rebelo Pires, 2018
-}

module Breakout.Scoring where

import Graphics.Gloss

import Breakout.AtomicDefinitions
import Breakout.Renderer

loseState :: GameState -> Bool
loseState (GameState bar ball _ _ _ _ _)
  = y'+ballRadius < y-1/2*barH
  where (Bar,((_,y),(_,_)))    = bar
        (Ball,((_',y'),(_,_))) = ball

scoreBlock :: Entity -> Int
scoreBlock (Block row, (_,_)) = round ((3.5 - row)*fromIntegral baseScore)
