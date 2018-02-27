{-
  Game renderer using Gloss Library
  João Rebelo Pires, 2018
-}

module Breakout.Renderer where

import Graphics.Gloss
import Breakout.AtomicDefinitions

-- | render all entities into a picture
render :: GameState -> Picture
render (GameState bar ball score blocks)
  = pictures (map renderEnt (bar:ball:score:blocks))


-- | render a single entity
renderEnt :: Entity -> Picture
renderEnt (shape, ((x,y), _))
  = translate x y $ renderShape shape


-- | render a shape in origin and with default heading
renderShape :: Shape -> Picture
renderShape Bar           = color blue bar
renderShape Ball          = color green ball
renderShape (Block (x,_)) = color white (scale x x block)
renderShape (Score s)     = color white (scale 0.15 0.15 (text txt))
  where txt = "Score: " ++ show(s)
