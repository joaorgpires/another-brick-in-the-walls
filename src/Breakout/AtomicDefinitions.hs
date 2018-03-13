{-
  Types and Data used throughout the project
  João Rebelo Pires, 2018
-}

module Breakout.AtomicDefinitions where

import Graphics.Gloss
import qualified Data.Map.Lazy as Map

-- | representation for the game state;
-- player's bar ball score level lives map of (y, blocks centered with yvalue = y)
data GameState = GameState Entity Entity Entity Entity Entity Blocks

data Shape = Bar        -- ^ bar
          | Ball        -- ^ ball
          | Block Float -- ^ block (type)
          | Score Int   -- ^ score
          | Level Int   -- ^ level
          | Lives Int   -- ^ number of lives, initially 3

-- | coordinates for movement calculations
-- position, velocity vector
type Coords = (Point,      -- ^ center position
               Vector     -- ^ linear velocity
              )

type Entity     = (Shape, Coords)

type Dimensions = (Float, Float) -- dimensions for blocks

type Blocks = Map.Map Float [Entity]

-- | objects dimensions
ballRadius, barW, barH, blockW, blockH :: Float
ballRadius = 10
barW       = 80
barH       = 20
blockW     = 80
blockH     = 20

-- | base score
baseScore :: Int
baseScore = 10

-- | objects as pictures
bar, ball, block :: Picture
bar   = rectangleSolid barW barH
ball  = circleSolid ballRadius
block = rectangleSolid blockW blockH

-- | display dimensions
maxWidth, maxHeight :: Float
maxWidth  = 800
maxHeight = 400
