{-
  Game renderer using Gloss Library
  João Rebelo Pires, 2018
-}

module Breakout.Renderer where

import Graphics.Gloss
import Breakout.AtomicDefinitions
import Data.Maybe
import qualified Data.Map.Lazy as Map

-- | render all entities into a picture
render :: GameState -> Picture
render (GameState bar ball score blocks)
  = pictures (map renderEnt (bar:ball:score:(blocksToList blocks)))


-- | render a single entity
renderEnt :: Entity -> Picture
renderEnt (shape, ((x,y), _))
  = translate x y $ renderShape shape


-- | render a shape in origin and with default heading
renderShape :: Shape -> Picture
renderShape Bar       = color blue bar
renderShape Ball      = color green ball
renderShape (Block x) = color white (scale x 1 block)
renderShape (Score s) = color white (scale 0.15 0.15 (text txt))
  where txt = "Score: " ++ show(s)

blocksToList :: Blocks -> [Entity]
blocksToList blocks = concat (map (\(_,y) -> y) (Map.toList blocks))
