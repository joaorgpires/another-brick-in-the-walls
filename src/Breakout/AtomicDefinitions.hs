{-
  Types and Data used throughout the project
  João Rebelo Pires, 2018
-}

module Breakout.AtomicDefinitions where

import Graphics.Gloss
import qualified Data.Map.Lazy as Map

-- | representation for the game state;
-- player's bar ball score level lives map of (y, blocks centered with yvalue = y)
data GameState = GameState Entity Entity Entity Entity Entity Blocks State

-- | each game object shape data
data Shape = Bar         -- ^ bar
           | Ball        -- ^ ball
           | Block Float -- ^ block (type)
           | Score Int   -- ^ score
           | Level Int   -- ^ level
           | Lives Int   -- ^ number of lives, initially 3
           | Loser       -- ^ loser text
           | Winner      -- ^ winner text
           | Controls    -- ^ controls text

-- | game state - Playing, Paused, New, Lose, Win
data State = Playing       -- Playing state -- ball moving
           | Paused Vector -- Velocity before pausing
           | New Vector    -- Velocity to use upon restarting
           | Lose          -- Player lost
           | Win           -- Player won

-- | coordinates for movement calculations
-- position, velocity vector
type Coords = (Point,      -- ^ center position
               Vector      -- ^ linear velocity
              )

-- | entity that has some shape and some coords for positioning
type Entity     = (Shape, Coords)

-- | type for the blocks storage
-- mapping some y to a list of blocks placed at that y
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
maxWidth, maxHeight, vxi, vyi :: Float
maxWidth  = 800
maxHeight = 400
vxi       = 180
vyi       = 180

-- | test of the state is Playing
isPlaying :: State -> Bool
isPlaying Playing = True
isPlaying _       = False

-- | test of the state is New
isNew :: State -> Bool
isNew (New _) = True
isNew _       = False

-- | simple floor function to avoid warnings
myFloor :: Float -> Int
myFloor x = floor x

-- | frames per second for game event loop
fps :: Int
fps = 60
