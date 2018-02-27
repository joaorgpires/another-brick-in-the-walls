{-
  Types and Data used throughout the project
  João Rebelo Pires, 2018
-}

module Breakout.AtomicDefinitions where

import Graphics.Gloss

-- | representation for the game state;
-- player's bar plus ball plus list of lists of blocks
data GameState = GameState Entity Entity Entity [Entity]

data Shape = Bar -- ^ bar
          | Ball -- ^ ball
          | Block Point -- ^ block (position)
          | Score Int

-- | coordinates for movement calculations
-- position, velocity vector
type Coords = (Point,      -- ^ center position
               Vector     -- ^ linear velocity
              )

type Entity     = (Shape, Coords)

type Dimensions = (Float, Float) -- dimensions for blocks

-- | objects dimensions
ballRadius, barW, barH :: Float
ballRadius = 10
barW       = 80
barH       = 20

-- | objects as pictures
bar, ball, block :: Picture
bar   = rectangleSolid barW barH
ball  = circleSolid ballRadius
block = rectangleSolid 80 20

-- | display dimensions
maxWidth, maxHeight :: Float
maxWidth  = 800
maxHeight = 400
