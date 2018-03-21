{-
  Game renderer using Gloss Library
  João Rebelo Pires, 2018
  Texts may not be completely centered -- since gloss text positions are not the center of the text
-}

module Breakout.Renderer where

import Graphics.Gloss
import Breakout.AtomicDefinitions
import qualified Data.Map.Lazy as Map

-- | render all entities into a picture
render :: GameState -> Picture
-- if the player lost, show appropriate message, stop playing
render (GameState _ _ score _ _ _ Lose)
  = pictures (map renderEnt (controls:score':[loser]))
    where (Score s, _) = score
          loser    = (Loser,((-120,-20),(0,0)))
          score'   = (Score s,((-120,-40),(0,0)))
          controls = (Controls,((-630,385),(0,0)))
-- if the player won, show appropriate message
render (GameState _ _ score _ _ _ Win)
  = pictures (map renderEnt (controls:score':[winner]))
    where (Score s, _) = score
          winner = (Winner,((-120,-20),(0,0)))
          score' = (Score s,((-120,-40),(0,0)))
          controls = (Controls,((-630,385),(0,0)))
-- simply render a gamestate while playing/in new life/level
render (GameState bar ball score level lives blocks _)
  = pictures (map renderEnt (controls:bar:ball:score:level:lives:(blocksToList blocks)))
    where controls = (Controls,((-630,385),(0,0)))


-- | render a single entity
renderEnt :: Entity -> Picture
renderEnt (shape, ((x,y), _))
  = translate x y $ renderShape shape


-- | render a shape in origin and with default heading
renderShape :: Shape -> Picture
renderShape Bar        = color blue bar
renderShape Ball       = color green ball
renderShape (Block x)  = color white (scale x 1 block)
renderShape (Score s)  = color white (scale 0.15 0.15 (text txt))
  where txt = "Score: " ++ show(s)
renderShape (Level l)  = color white (scale 0.15 0.15 (text txt))
  where txt = "Level: " ++ show(l)
renderShape (Lives l)  = color white (scale 0.15 0.15 (text txt))
  where txt = "Lives: " ++ show(l)
renderShape (Loser)    = color white (scale 0.5 0.5 (text txt))
  where txt = "LOSER!"
renderShape (Winner)   = color white (scale 0.5 0.5 (text txt))
  where txt = "WINNER!"
renderShape (Controls) = color white (scale 0.1 0.1 (text txt))
  where txt = "N: New Game     P: Pause/Unpause     Space: Start level/life"
              ++ "     Left: Move bar left     Right: Move bar right"
              ++ "     Up: Start to the left     Down: Start to the right (default)"

-- | get all the blocks on screen, stored in the map blocks
blocksToList :: Blocks -> [Entity]
blocksToList blocks = concat (map (\(_,y) -> y) (Map.toList blocks))
