{-
  Simple Breakout-like game using Gloss Library
  João Rebelo Pires, 2018
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Breakout.AtomicDefinitions
import Breakout.Renderer
import Breakout.Scoring

-- | advance coordinates by a time delta
advanceCoords :: Float -> Float -> Coords -> Coords
advanceCoords dt radius ((x,y),(dx,dy))
  = ((x',y'),(dx',dy'))
  where (x',dx') = clip x dx (maxWidth-radius)
        (y',dy') = clip y dy (maxHeight-radius)
        -- clip to a bounding interval -- from gloss-demo example
        clip h dh max
          | h' > max  = (max,-dh)
          | h' < -max = (-max,-dh)
          | otherwise = (h',dh)
          where h' = h + dt*dh

-- | advance an entity by a time delta
advanceEnt :: Float -> Entity -> Entity
advanceEnt dt (Ball,mov) = (Ball,advanceCoords dt ballRadius mov)
advanceEnt dt (Bar, mov) = (Bar,advanceCoords dt (barW / 2) mov)

stop :: GameState -> GameState
stop (GameState bar ball score other)
  = GameState bar' ball' score other
  where (Bar,((x,y),(_,_))) = bar
        (Ball,((x',y'),(_,_))) = ball
        bar' = (Bar,((x,y),(0,0)))
        ball' = (Ball,((x',y'),(0,0)))

-- | advance all game entities by a time delta
advance :: Float -> GameState -> GameState
advance dt (GameState bar ball score other)
  | loseState (GameState bar ball score other) = stop (GameState bar ball score other)
  | otherwise = GameState (advanceEnt dt bar) (advanceEnt dt ball) score other

-- | update the game state;
-- time delta, decay laser and check for colitions
update :: Float -> GameState -> GameState -- Change this afterwards
update dt game
  = collisions (advance dt game)


-- | collision detection
collisions :: GameState -> GameState
collisions (GameState bar ball score other)
  = GameState bar ball' score other
  where (Ball,((x,y),(dx,dy))) = ball
        (Bar,((_,y'),(_,_))) = bar
        ball'
          | ball `hits` bar = (Ball,((x,y'+1/2*barH+ballRadius),(dx,-dy)))
          | otherwise = (Ball,((x,y),(dx,dy)))


-- | check colision between two entities
hits :: Entity -> Entity -> Bool
hits (Ball,((x,y),_)) (Bar,((x',y'),_))
  = (ballRadius+1/2*barH >= y-y') && (x'-1/2*barW <= x) && (x <= x'+1/2*barW) -- == not working for some reason? does it have to do with dt?
hits _ _ = False

-- | react to keyboard events
react :: Event -> GameState -> GameState
-- move bar (left/right)
react (EventKey (SpecialKey KeyLeft) keystate _ _) (GameState bar ball score blocks)
  = GameState bar' ball score blocks
  where (Bar, (pos,(_,dy))) = bar
        dx'  = if keystate==Down then -150 else 0
        bar' = (Bar, (pos, (dx',dy)))

react (EventKey (SpecialKey KeyRight) keystate _ _) (GameState bar ball score blocks)
  = GameState bar' ball score blocks
  where (Bar, (pos, (_,dy))) = bar
        dx'  = if keystate==Down then 150 else 0
        bar' = (Bar, (pos, (dx', dy)))
-- ignore all other keys and events
react (EventKey (Char 'n') Down _ _) _
  = initialState []
react _ world = world

-- | frames per second for game event loop
fps :: Int
fps = 60

-- | initial game state
initialState :: [Entity] -> GameState
initialState blocks = GameState bar ball score blocks
  where
    bar   = (Bar, ((0,-350), (0,0)))
    ball  = (Ball, ((0,-330), (80,80)))
    score = (Score 0, ((600,-390),(0,0)))

-- | main entry point
main :: IO ()
main = do
 play window black fps (initialState []) render react update

window :: Display
window = InWindow "Breakout" (2*round maxWidth,2*round maxHeight) (0,0)
