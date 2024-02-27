module Simulator where

import Control.Monad
import Graphics.Gloss
import Simulation

--type Point = (Float, Float)

renderAnimal :: Animal -> Point -> Picture
renderAnimal (Fox _) (x,y) = Translate x y $ Color orange $ circleSolid 2
renderAnimal (Rabbit _) (x,y) = Translate x y $ Color white $ circleSolid 2

renderAnimals :: [Animal] -> Picture
renderAnimals animals = pictures $ zipWith renderAnimal (take 4 animals) positions
  where 
    positions = [ (-10, 10), (10, 10), (-10, -10), (10, -10) ]
    
renderSim :: World (Environment Animal) ->  Picture
renderSim wrld = pictures [ renderCell i j | i <- [0..dim-1], j <- [0..dim-1] ]
  where
    mapData = getMap wrld
    dim = length mapData
    renderCell i j = 
      let x = (fromIntegral j * 50) - 250 in
      let y = (fromIntegral i * 50) - 250 in
      let color = black in
      Translate x y $ pictures [(Color color $ rectangleSolid 50 50), renderAnimals (mapData !! i !! j)]

getSimulator :: IO ()
getSimulator = play
  (InWindow "Simulator" (500, 500) (0, 0))
  white
  1
  (newSimulation 10 10 10) 
  renderSim
  (\_ state -> state)
  (\_ state -> simulateNext state)

