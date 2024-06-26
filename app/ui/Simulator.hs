module Simulator where

import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Maybe
import Simulation
import System.Random

-- to avoid magic number
foxindex = 0
babyfoxindex  = 1
rabbitindex = 2
babyrabbitindex  = 3
grassindex = 4
waterindex = 5

-- returns appropriate Animal sprite based on type
renderAnimal :: [Picture] -> Animal -> Point -> Picture
renderAnimal pics (Fox _) (x,y) = Translate x y $ pics !! foxindex
renderAnimal pics (Rabbit _) (x,y) = Translate x y $ pics !! rabbitindex

-- returns appropriate Resource sprite based on type
renderResource :: [Picture] -> Resource -> Point -> Picture
renderResource pics (Grass _ _) (x,y) = Translate x y $ pics !! grassindex
renderResource pics (Water _ _) (x,y) = Translate x y $ pics !! waterindex

-- renders the first 9 resources at a given (x,y) pos, then
-- renders the first 9 animals   at the same position, on top
renderEithers :: [Picture] -> [Either Animal Resource] -> Picture
renderEithers pics eithers = pictures ((zipWith (renderResource pics) (take 9 resources) positions)
                                      ++ (zipWith (renderAnimal pics)   (take 9 animals)   positions))
  where
    positions = [(x, y) | y <- [-10,0,10], x <- [10,0,-10]]
    resources = [resource | Right resource <- eithers]
    animals   = [animal   | Left  animal   <- eithers]

-- renders the simulation after each cycle
renderSim :: Int -> [Picture] -> World (Environment (Either Animal Resource)) ->  Picture
renderSim size pics wrld = pictures [ renderCell i j | i <- [0..dim-1], j <- [0..dim-1] ]
  where
    mapData = getMap wrld
    dim = length mapData
    renderCell i j = 
      let x = (fromIntegral j * 30) - (fromIntegral size / 2) + 15 in
      let y = (fromIntegral i * 30) - (fromIntegral size / 2) + 15 in
      Translate x y $ pictures [Color black $ rectangleSolid 30 30, renderEithers pics (mapData !! i !! j)]

-- main function
-- creates a new simulation with the given:
-- 1. dimensions (NxNxN)
-- 2. turns remaining
-- 3. number of entities (Animal | Resource)
-- returns: a window displaying the simulation
getSimulator :: Int -> Int -> IO ()
getSimulator dim num = do

  -- std random generator for randomness
  gen <- newStdGen
 
  -- load PNG sprites from assets directory
  adultFoxSprite    <- loadJuicyPNG "assets/adultfox10.png"
  babyFoxSprite     <- loadJuicyPNG "assets/babyfox.png"
  adultRabbitSprite <- loadJuicyPNG "assets/adultrabbit.png"
  babyRabbitSprite  <- loadJuicyPNG "assets/babyrabbit.png"
  grassSprite       <- loadJuicyPNG "assets/grass10.png"
  waterSprite       <- loadJuicyPNG "assets/water10.png"
  
  -- Gloss function, plays the simulation in a window
  -- window size is derived from simulation dimentions
  play 
    (InWindow "Simulator" (size, size) (20, 20))
    white 
    1 

    (newSimulation dim 0 num gen) 
    (renderSim size [fromMaybe (color orange $ circleSolid 10) adultFoxSprite, 
                     fromMaybe (color orange $ circleSolid 5) babyFoxSprite,
                     fromMaybe (color white  $ circleSolid 5) adultRabbitSprite,
                     fromMaybe (color white  $ circleSolid 2) babyRabbitSprite,
                     fromMaybe (color green  $ rectangleSolid 10 10) grassSprite,
                     fromMaybe (color blue   $ rectangleSolid 10 10) waterSprite])
    (\_ state -> state)
    (\_ state -> simulateNext state)
  where
    size = dim * 30
