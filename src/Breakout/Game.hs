{-
  Game module using Gloss Library
  João Rebelo Pires, 2018
-}

module Breakout.Game where

import Graphics.Gloss
import qualified Data.Map.Lazy as Map
import Data.Maybe as Maybe

import Breakout.AtomicDefinitions
import Breakout.Renderer

loseState :: GameState -> Bool
loseState (GameState bar ball _ _ _ _ _)
  = y'+ballRadius < y-1/2*barH
  where (Bar,((_,y),(_,_)))    = bar
        (Ball,((_',y'),(_,_))) = ball

scoreBlock :: Entity -> Int
scoreBlock (Block row, (_,_)) = round ((3.5 - row)*fromIntegral baseScore)

-- | advance ball coordinates by a time delta
advanceBall :: State -> Float -> Float -> Coords -> Coords
advanceBall state dt radius ((x,y),(dx,dy))
  = if isNew state then advanceBar dt (barW / 2) ((x,y),(dx,dy)) else ((x',y'),(dx',dy'))
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
advanceEnt :: State -> Float -> Entity -> Entity
advanceEnt state dt (Ball,mov) = (Ball,advanceBall state dt ballRadius mov)
advanceEnt _ dt (Bar, mov)     = (Bar,advanceBar dt (barW / 2) mov)

-- | advance all game entities by a time delta
advance :: Float -> GameState -> GameState
advance dt (GameState bar ball score level lives blocks state)
  | loseState (GameState bar ball score level lives blocks state) = if (liv == 1) then (GameState bar ball score level lives blocks Lose)
                                                                    else initialState (getVelocity lev) sc lev (liv - 1) blocks
  | Map.null blocks = if lev == 5 then (GameState bar ball score level lives blocks Win)
                      else initialState (getVelocity (lev+1)) sc (lev + 1) liv (genBlocks (lev+1) 2.5)
  | otherwise       = GameState (advanceEnt state dt bar) (advanceEnt state dt ball) score level lives blocks state
  where (Score sc,_)                            = score
        (Level lev,_) = level
        (Lives liv,_) = lives

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
                                       else (-3000,[])
        f _                          = if (null blockList') then Nothing else Just blockList'
        blocks'                      = Map.update f yblock blocks
        (score',blockList')          = removeBlock score blockList
        f' _                         = if (null blockListAbove') then Nothing else Just blockListAbove'
        blocks''                     = if (yblockAbove == -3000) || (yblock == yblockAbove) then blocks'
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
collisions (GameState bar ball score level lives blocks state)
  = GameState bar ball' score' level lives' blocks' state
  where (Ball,((x,y),(dx,dy)))                  = ball
        (Bar,((x',y'),(_,_)))                    = bar
        (Score s,((sx,sy),(sdx,sdy)))           = score
        (Lives liv,((livx,livy),(livdx,livdy))) = lives
        ball'
          | ball `hits` bar                                      = (Ball,((x,y'+1/2*barH+ballRadius),(dx',-dy)))
          | or (map (\z -> ball `hits` z) (blocksToList blocks)) = (Ball,((x,y),(dx,-dy)))
          | otherwise                                            = (Ball,((x,y),(dx,dy)))
        (s',blocks') = blocksCollisions s ball blocks
        score' = (Score s',((sx,sy),(sdx,sdy)))
        lives' = if (s' /= s && (myFloor (fromIntegral s/1500)) /= (myFloor (fromIntegral s'/1500))) then (Lives (liv+1),((livx,livy),(livdx,livdy)))
                 else lives
        dx'
          | (isPlaying state) = if x <= x' then -(abs dx) else (abs dx)
          | otherwise         = dx

-- | check colision between two entities
hits :: Entity -> Entity -> Bool
hits (Ball,((x,y),_)) (Bar,((x',y'),_))
  = (ballRadius+1/2*barH >= y-y') && (x'-1/2*barW <= x) && (x <= x'+1/2*barW)
hits (Ball,((x,y),_)) (Block row,((x',y'),_))
  = (ballRadius+1/2*blockH >= abs (y'-y)) && (x'-1/2*blockW*row <= x) && (x <= x'+1/2*blockW*row)
hits _ _ = False

-- | initial game state
initialState :: Vector -> Int -> Int -> Int -> Blocks -> GameState
initialState (dx,dy) sc lev liv blocks = GameState bar ball score level lives blocks (New (dx,dy))
  where
    bar   = (Bar, ((0,-350), (0,0)))
    ball  = (Ball, ((0,-330), (0,0)))
    score = (Score sc, ((510,-390),(0,0)))
    level = (Level lev, ((-35,-390),(0,0)))
    lives = (Lives liv, ((-600,-390),(0,0)))

getBlock :: Float -> Float -> Float -> Entity
getBlock row x y
  = (Block row, ((x,y),(0,0)))

genRow :: Int -> Float -> (Float, [Entity])
genRow nb row = (y, map (\x -> getBlock row x y) [-maxWidth + blockSize / 2, -maxWidth + blockSize + intervalSize + blockSize / 2..maxWidth])
  where y             = maxHeight - 25 - 3 * (row * 50) - (fromIntegral nb)*25
        blockSize     = row * blockW
        nblocks       = myFloor (2*maxWidth / blockSize - 1)
        intervalSize  = (2*maxWidth - fromIntegral nblocks*blockSize) / (fromIntegral nblocks - 1)

genBlocks :: Int -> Float -> Blocks
genBlocks lvl n = if (fromIntegral (6-lvl))*0.5 > n then Map.empty
                  else m1
  where (y1, blockList1) = genRow 1 n
        (y2, blockList2) = genRow 2 n
        (y3, blockList3) = genRow 3 n
        m3 = Map.insert y3 blockList3 (genBlocks lvl (n - 0.5))
        m2 = Map.insert y2 blockList2 m3
        m1 = Map.insert y1 blockList1 m2

getVelocity :: Int -> Vector
getVelocity lvl = (vxi + (fromIntegral (lvl - 1)) * 20,vyi + (fromIntegral (lvl - 1)) * 20)

firstLevel :: GameState
firstLevel = initialState (getVelocity 1) 0 1 3 (genBlocks 1 2.5)
