module Simulator where

import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Maybe
import Simulation
import System.Random

foxindex = 0
babyfoxindex  = 1
rabbitindex = 2
babyrabbitindex  = 3
grassindex = 4
waterindex = 5

renderAnimal :: [Picture] -> Animal -> Point -> Picture
renderAnimal pics (Fox _) (x,y) = Translate x y $ pics !! foxindex
renderAnimal pics (Rabbit _) (x,y) = Translate x y $ pics !! rabbitindex

renderResource :: [Picture] -> Resource -> Point -> Picture
renderResource pics (Grass _ _) (x,y) = Translate x y $ pics !! grassindex
renderResource pics (Water _ _) (x,y) = Translate x y $ pics !! waterindex

renderEithers :: [Picture] -> [Either Animal Resource] -> Picture
renderEithers pics eithers = pictures ((zipWith (renderResource pics) (take 9 resources) positions)
                                      ++ (zipWith (renderAnimal pics)   (take 9 animals)   positions))
  where
    positions = [(x, y) | y <- [-10,0,10], x <- [10,0,-10]]
    resources = [resource | Right resource <- eithers]
    animals   = [animal   | Left  animal   <- eithers]

renderSim :: Int -> [Picture] -> World (Environment (Either Animal Resource)) ->  Picture
renderSim size pics wrld = pictures [ renderCell i j | i <- [0..dim-1], j <- [0..dim-1] ]
  where
    mapData = getMap wrld
    dim = length mapData
    renderCell i j = 
      let x = (fromIntegral j * 30) - (fromIntegral size / 2) + 15 in
      let y = (fromIntegral i * 30) - (fromIntegral size / 2) + 15 in
      Translate x y $ pictures [Color black $ rectangleSolid 30 30, renderEithers pics (mapData !! i !! j)]

getSimulator :: IO ()
getSimulator = do

  gen <- newStdGen
  
  adultFoxSprite    <- loadJuicyPNG "assets/adultfox10.png"
  babyFoxSprite     <- loadJuicyPNG "assets/babyfox.png"
  adultRabbitSprite <- loadJuicyPNG "assets/adultrabbit.png"
  babyRabbitSprite  <- loadJuicyPNG "assets/babyrabbit.png"
  grassSprite       <- loadJuicyPNG "assets/grass10.png"
  waterSprite       <- loadJuicyPNG "assets/water10.png"
  
  play 
    (InWindow "Simulator" (size, size) (20, 20))
    white 
    1 
    (newSimulation dim 100 600 gen) 
    (renderSim size [fromMaybe (color orange $ circleSolid 10) adultFoxSprite, 
                     fromMaybe (color orange $ circleSolid 5) babyFoxSprite,
                     fromMaybe (color white  $ circleSolid 5) adultRabbitSprite,
                     fromMaybe (color white  $ circleSolid 2) babyRabbitSprite,
                     fromMaybe (color green  $ rectangleSolid 10 10) grassSprite,
                     fromMaybe (color blue   $ rectangleSolid 10 10) waterSprite])
    (\_ state -> state)
    (\_ state -> simulateNext state)
  where
    dim = 20
    size = dim * 30
