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
stop (GameState bar ball score level lives blocks)
  = GameState bar' ball' score level lives blocks
  where (Bar,((x,y),(_,_)))    = bar
        (Ball,((x',y'),(_,_))) = ball
        bar'  = (Bar,((x,y),(0,0)))
        ball' = (Ball,((x',y'),(0,0)))

-- | advance all game entities by a time delta
advance :: Float -> GameState -> GameState
advance dt (GameState bar ball score level lives blocks)
  | loseState (GameState bar ball score level lives blocks) = if (liv == 0 || liv == 1) then stop (GameState bar ball score level lives' blocks)
                                                              else initialState sc lev (liv - 1) blocks
  | Map.null blocks = if lev == 5 then stop (GameState bar ball score level lives blocks)
                      else initialState sc (lev + 1) liv (genBlocks 2.5)
  | otherwise       = GameState (advanceEnt dt bar) (advanceEnt dt ball) score level lives blocks
  where (Score sc,_)                            = score
        (Level lev,_) = level
        (Lives liv,((livx,livy),(livdx,livdy))) = lives
        lives' = (Lives 0,((livx,livy),(livdx,livdy)))
        
-- | update the game state;
-- time delta, decay laser and check for colitions
update :: Float -> GameState -> GameState
update dt game
  = collisions (advance dt game)

-- | blocks collisions
blocksCollisions :: Int -> Entity -> Blocks -> (Int,Blocks)
blocksCollisions score ball blocks
  | not (or (map (\z -> ball `hits` z) (blocksToList blocks))) = (score,blocks)
  | otherwise = if (length blockList == 1 && ball `hits` (head blockList)) then (score + scoreBlock (head blockList),Map.delete yblock blocks)
                else (sc,blocks'')
  where (Ball,((_,y),_))             = ball
        (yblock,blockList)           = Maybe.fromJust (Map.lookupGE (y-1/2*blockH - ballRadius)  blocks)
        (yblockAbove,blockListAbove) = if (Maybe.isJust (Map.lookupGE y blocks)) then Maybe.fromJust (Map.lookupGE y blocks)
                                       else (y,[])
        f _                          = Just blockList'
        blocks'                      = Map.update f yblock blocks
        (score',blockList')          = removeBlock score blockList
        f' _                         = Just blockListAbove'
        blocks''                     = if (null blockListAbove) || (yblock == yblockAbove) then blocks'
                                       else Map.update f' yblockAbove blocks'
        (score'',blockListAbove')    = removeBlock score' blockListAbove
        sc                           = if (null blockListAbove) || (yblock == yblockAbove) then score'
                                       else score''
        removeBlock s []             = (s, [])
        removeBlock s (el:lst)       = if ball `hits` el then removeBlock (s + (scoreBlock el)) lst
                                       else (sc',el:lst')
                                          where (sc',lst') = removeBlock s lst

-- | collision detection
collisions :: GameState -> GameState
collisions (GameState bar ball score level lives blocks)
  = GameState bar ball' score' level lives' blocks'
  where (Ball,((x,y),(dx,dy)))                  = ball
        (Bar,((_,y'),(_,_)))                    = bar
        (Score s,((sx,sy),(sdx,sdy)))           = score
        (Lives liv,((livx,livy),(livdx,livdy))) = lives
        ball'
          | ball `hits` bar                                      = (Ball,((x,y'+1/2*barH+ballRadius),(dx,-dy)))
          | or (map (\z -> ball `hits` z) (blocksToList blocks)) = (Ball,((x,y),(dx,-dy)))
          | otherwise                                            = (Ball,((x,y),(dx,dy)))
        (s',blocks') = blocksCollisions s ball blocks
        score' = (Score s',((sx,sy),(sdx,sdy)))
        lives' = if (s' /= s && (myFloor (fromIntegral s/1500)) /= (myFloor (fromIntegral s'/1500))) then (Lives (liv+1),((livx,livy),(livdx,livdy)))
                 else lives
        

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
react (EventKey (SpecialKey KeyLeft) keystate _ _) (GameState bar ball score level lives blocks)
  = GameState bar' ball score level lives blocks
  where (Bar, (pos,(_,dy))) = bar
        dx'  = if keystate==Down then -300 else 0
        bar' = (Bar, (pos, (dx',dy)))

react (EventKey (SpecialKey KeyRight) keystate _ _) (GameState bar ball score level lives blocks)
  = GameState bar' ball score level lives blocks
  where (Bar, (pos, (_,dy))) = bar
        dx'  = if keystate==Down then 300 else 0
        bar' = (Bar, (pos, (dx', dy)))
-- ignore all other keys and events
react (EventKey (Char 'n') Down _ _) _ = initialState 0 1 3 (genBlocks 2.5)
react _ world                          = world

-- | frames per second for game event loop
fps :: Int
fps = 60

-- | initial game state
initialState :: Int -> Int -> Int -> Blocks -> GameState
initialState sc lev liv blocks = GameState bar ball score level lives blocks
  where
    bar   = (Bar, ((0,-350), (0,0)))
    ball  = (Ball, ((0,-330), (180,180)))
    score = (Score sc, ((600,-390),(0,0)))
    level = (Level lev, ((0,-390),(0,0)))
    lives = (Lives liv, ((-600,-390),(0,0)))

getBlock :: Float -> Float -> Float -> Entity
getBlock row x y
  = (Block row, ((x,y),(0,0)))

myFloor :: Float -> Int
myFloor x = floor x

genRow :: Float -> (Float, [Entity])
genRow row = (y, map (\x -> getBlock row x y) [-maxWidth + blockSize / 2, -maxWidth + blockSize + intervalSize + blockSize / 2..maxWidth])
  where y             = maxHeight - 225 - row * 50
        blockSize     = row * blockW
        nblocks       = myFloor (2*maxWidth / blockSize - 1)
        intervalSize  = (2*maxWidth - fromIntegral nblocks*blockSize) / (fromIntegral nblocks - 1)

genBlocks :: Float -> Blocks
genBlocks 0 = Map.empty
genBlocks n = Map.insert y blockList (genBlocks (n - 0.5))
  where (y, blockList) = genRow n


-- | main entry point
main :: IO ()
main = do
 play window black fps (initialState 0 5 3 (genBlocks 2.5)) render react update

window :: Display
window = InWindow "Breakout" (2*round maxWidth,2*round maxHeight) (0,0)
