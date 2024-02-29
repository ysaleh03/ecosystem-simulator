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

getSprite :: [Picture] -> Either Animal Resource -> Picture
getSprite pics (Left (Fox _)) = pics !! foxindex
getSprite pics (Left (Rabbit _)) = pics !! rabbitindex
getSprite pics (Right (Grass _ _)) = pics !! grassindex
getSprite pics (Right (Water _ _)) = pics !! waterindex
getSprite _ _ = color white $ rectangleSolid 10 10 

renderEither :: [Picture] -> Either Animal Resource -> Point -> Picture
renderEither pics either (x,y) = Translate x y $ getSprite pics either

renderEithers :: [Picture] -> [Either Animal Resource] -> Picture
renderEithers pics eithers = pictures $ zipWith (renderEither pics) (take 9 eithers) positions
  where
    positions = [(x, y) | x <- [-40, -30 .. 40], y <- [40, 30 .. -40]]

renderSim :: [Picture] -> World (Environment (Either Animal Resource)) ->  Picture
renderSim pics wrld = pictures [ renderCell i j | i <- [0..dim-1], j <- [0..dim-1] ]
  where
    mapData = getMap wrld
    dim = length mapData
    renderCell i j = 
      let x = (fromIntegral j * 90) - 450 + 45 in
      let y = (fromIntegral i * 90) - 450 + 45 in
      let color = black in
      Translate x y $ pictures [Color color $ rectangleSolid 90 90, renderEithers pics (mapData !! i !! j)]

getSimulator :: IO ()
getSimulator = do

  gen <- newStdGen
  
  adultFoxSprite    <- loadJuicyPNG "assets/adultfox10.png"
  babyFoxSprite     <- loadJuicyPNG "assets/babyfox.png"
  adultRabbitSprite <- loadJuicyPNG "assets/adultrabbit.png"
  babyRabbitSprite  <- loadJuicyPNG "assets/babyrbbit.png"
  grassSprite       <- loadJuicyPNG "assets/grass10.png"
  waterSprite       <- loadJuicyPNG "assets/water10.png"
  
  play 
    (InWindow "Simulator" (900, 900) (20, 20))
    white 
    1 
    (newSimulation 10 0 20 gen) 
    (renderSim [ fromMaybe (color orange $ circleSolid 10) adultFoxSprite, 
                 fromMaybe (color orange $ circleSolid 5) babyFoxSprite,
                 fromMaybe (color white  $ circleSolid 5) adultRabbitSprite,
                 fromMaybe (color white  $ circleSolid 2) babyRabbitSprite,
                 fromMaybe (color green  $ rectangleSolid 10 10) grassSprite,
                 fromMaybe (color blue   $ rectangleSolid 10 10) waterSprite ])
    (\_ state -> state)
    (\_ state -> simulateNext state)

