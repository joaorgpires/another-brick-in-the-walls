{-
  Simple Breakout-like game using Gloss Library
  João Rebelo Pires, 2018
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map.Lazy as Map
import Data.Maybe as Maybe

import Breakout.AtomicDefinitions
import Breakout.Renderer
import Breakout.Scoring

-- | advance ball coordinates by a time delta
advanceBall :: Float -> Float -> Coords -> Coords
advanceBall dt radius ((x,y),(dx,dy))
  = ((x',y'),(dx',dy'))
  where (x',dx') = clip x dx (maxWidth-radius)
        (y',dy') = clip y dy (maxHeight-radius)
        -- clip to a bounding interval -- from gloss-demo example
        clip h dh max
          | h' > max  = (max,-dh)
          | h' < -max = (-max,-dh)
          | otherwise = (h',dh)
          where h' = h + dt*dh

-- | advance bar coordinates by a time delta
advanceBar :: Float -> Float -> Coords -> Coords
advanceBar dt radius ((x,y),(dx,dy))
  = ((x',y'),(dx',dy'))
  where (x',dx') = clip x dx (maxWidth-radius)
        (y',dy') = clip y dy (maxHeight-radius)
        -- clip to a bounding interval
        clip h dh max
          | h' > max  = (max,0)
          | h' < -max = (-max,0)
          | otherwise = (h',dh)
          where h' = h + dt*dh

-- | advance an entity by a time delta
advanceEnt :: Float -> Entity -> Entity
advanceEnt dt (Ball,mov) = (Ball,advanceBall dt ballRadius mov)
advanceEnt dt (Bar, mov) = (Bar,advanceBar dt (barW / 2) mov)

stop :: GameState -> GameState
stop (GameState bar ball score other)
  = GameState bar' ball' score other
  where (Bar,((x,y),(_,_)))    = bar
        (Ball,((x',y'),(_,_))) = ball
        bar'  = (Bar,((x,y),(0,0)))
        ball' = (Ball,((x',y'),(0,0)))

-- | advance all game entities by a time delta
advance :: Float -> GameState -> GameState
advance dt (GameState bar ball score blocks)
  | loseState (GameState bar ball score blocks) = stop (GameState bar ball score blocks)
  | Map.null blocks = stop (GameState bar ball score blocks)
  | otherwise       = GameState (advanceEnt dt bar) (advanceEnt dt ball) score blocks

-- | update the game state;
-- time delta, decay laser and check for colitions
update :: Float -> GameState -> GameState -- Change this afterwards
update dt game
  = collisions (advance dt game)

-- | blocks collisions
blocksCollisions :: Entity -> Blocks -> Blocks
blocksCollisions ball blocks
  | not (or (map (\z -> ball `hits` z) (blocksToList blocks))) = blocks
  | otherwise = if length blockList == 1 then Map.delete yblock blocks
                else blocks''
  where (Ball,((_,y),_))             = ball
        (yblock,blockList)           = Maybe.fromJust (Map.lookupGE (y-1/2*blockH - ballRadius)  blocks)
        (yblockAbove,blockListAbove) = if (Maybe.isJust (Map.lookupGE y blocks)) then Maybe.fromJust (Map.lookupGE y blocks)
                                       else (y,[])
        f _                          = Just blockList'
        blocks'                      = Map.update f yblock blocks
        blockList'                   = removeBlock blockList
        f' _                         = Just blockListAbove'
        blocks''                     = if (null blockListAbove) then blocks'
                                       else Map.update f' yblockAbove blocks'
        blockListAbove'              = removeBlock blockListAbove
        removeBlock []               = []
        removeBlock (el:lst)         = if ball `hits` el then removeBlock(lst)
                                       else el:(removeBlock lst)

-- | collision detection
collisions :: GameState -> GameState
collisions (GameState bar ball score blocks)
  = GameState bar ball' score blocks'
  where (Ball,((x,y),(dx,dy))) = ball
        (Bar,((_,y'),(_,_)))   = bar
        ball'
          | ball `hits` bar                                      = (Ball,((x,y'+1/2*barH+ballRadius),(dx,-dy)))
          | or (map (\z -> ball `hits` z) (blocksToList blocks)) = (Ball,((x,y),(dx,-dy)))
          | otherwise                                            = (Ball,((x,y),(dx,dy)))
        blocks' = blocksCollisions ball blocks


-- | check colision between two entities
hits :: Entity -> Entity -> Bool
hits (Ball,((x,y),_)) (Bar,((x',y'),_))
  = (ballRadius+1/2*barH >= y-y') && (x'-1/2*barW <= x) && (x <= x'+1/2*barW) -- == not working for some reason? does it have to do with dt?
hits (Ball,((x,y),_)) (Block row,((x',y'),_))
  = (ballRadius+1/2*blockH >= abs (y'-y)) && (x'-1/2*blockW*row <= x) && (x <= x'+1/2*blockW*row)
hits _ _ = False

-- | react to keyboard events
react :: Event -> GameState -> GameState
-- move bar (left/right)
react (EventKey (SpecialKey KeyLeft) keystate _ _) (GameState bar ball score blocks)
  = GameState bar' ball score blocks
  where (Bar, (pos,(_,dy))) = bar
        dx'  = if keystate==Down then -300 else 0
        bar' = (Bar, (pos, (dx',dy)))

react (EventKey (SpecialKey KeyRight) keystate _ _) (GameState bar ball score blocks)
  = GameState bar' ball score blocks
  where (Bar, (pos, (_,dy))) = bar
        dx'  = if keystate==Down then 300 else 0
        bar' = (Bar, (pos, (dx', dy)))
-- ignore all other keys and events
react (EventKey (Char 'n') Down _ _) _ = initialState (Map.empty)
react _ world                          = world

-- | frames per second for game event loop
fps :: Int
fps = 60

-- | initial game state
initialState :: Blocks -> GameState
initialState blocks = GameState bar ball score blocks
  where
    bar   = (Bar, ((0,-350), (0,0)))
    ball  = (Ball, ((0,-330), (150,150)))
    score = (Score 0, ((600,-390),(0,0)))

getBlock :: Float -> Float -> Float -> Entity
getBlock row x y
  = (Block row, ((x,y),(0,0)))

myFloor :: Float -> Int
myFloor x = floor x

genRow :: Float -> (Float, [Entity])
genRow row = (y, map (\x -> getBlock row x y) [-maxWidth + blockSize / 2, -maxWidth + blockSize + intervalSize + blockSize / 2..maxWidth])
  where y             = -75 + row * 50
        blockSize     = row * blockW
        nblocks       = myFloor (2*maxWidth / blockSize - 1)
        intervalSize  = (2*maxWidth - fromIntegral nblocks*blockSize) / (fromIntegral nblocks - 1)

genBlocks :: Float -> Blocks
genBlocks 0 = Map.empty
genBlocks n = Map.insert y blockList (genBlocks (n - 1))
    where (y, blockList) = genRow n


-- | main entry point
main :: IO ()
main = do
 play window black fps (initialState (genBlocks 2)) render react update

window :: Display
window = InWindow "Breakout" (2*round maxWidth,2*round maxHeight) (0,0)
