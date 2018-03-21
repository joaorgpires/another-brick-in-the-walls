{-
  Keys module using Gloss Library
  João Rebelo Pires, 2018
-}

module Breakout.Keys where

import Graphics.Gloss.Interface.Pure.Game

import Breakout.AtomicDefinitions
import Breakout.Game

-- | react to keyboard events
react :: Event -> GameState -> GameState
-- move bar (left/right)
react (EventKey (SpecialKey KeyLeft) keystate _ _) (GameState bar ball score level lives blocks state)
  = GameState bar' ball' score level lives blocks state
  where (Bar, (pos,(_,dy))) = bar
        dx'   = if keystate==Down && (isPlaying state || isNew state) then -300 else 0
        bar'  = (Bar, (pos, (dx',dy)))
        ball' = if isNew state then (Ball, (pos, (dx',dy))) else ball
react (EventKey (SpecialKey KeyRight) keystate _ _) (GameState bar ball score level lives blocks state)
  = GameState bar' ball' score level lives blocks state
  where (Bar, (pos, (_,dy))) = bar
        dx'  = if keystate==Down && (isPlaying state || isNew state) then 300 else 0
        bar' = (Bar, (pos, (dx', dy)))
        ball' = if isNew state then (Ball, (pos, (dx',dy))) else ball
-- new game (star at level 1) -- key n
react (EventKey (Char 'n') Down _ _) _    = firstLevel
-- pause or unpause the game -- key p
react (EventKey (Char 'p') Down _ _) (GameState bar ball score level lives blocks Playing)
  = GameState bar' ball' score level lives blocks (Paused (dx',dy'))
  where (Bar,((x,y),(_,_)))        = bar
        (Ball,((x',y'),(dx',dy'))) = ball
        bar'  = (Bar,((x,y),(0,0)))
        ball' = (Ball,((x',y'),(0,0)))
react (EventKey (Char 'p') Down _ _) (GameState bar ball score level lives blocks (Paused (dx,dy)))
  = GameState bar ball' score level lives blocks Playing
  where (Ball,((x,y),_)) = ball
        ball' = (Ball,((x,y),(dx,dy)))
-- start level/life -- space
react (EventKey (SpecialKey KeySpace) Down _ _) (GameState bar ball score level lives blocks (New (dx,dy)))
  = GameState bar ball' score level lives blocks Playing
  where (Ball, ((x,y),_)) = ball
        ball' = (Ball, ((x,y),(dx,dy)))
react (EventKey (SpecialKey KeyDown) Down _ _) (GameState bar ball score level lives blocks (New (dx,dy))) -- start to the right
  = GameState bar ball score level lives blocks (New (abs(dx),dy))
react (EventKey (SpecialKey KeyUp) Down _ _) (GameState bar ball score level lives blocks (New (dx,dy))) -- start to the left
  = GameState bar ball score level lives blocks (New (-abs(dx),dy))
  -- ignore all other keys and events
react _ world                             = world
