module Simulation (
  newSimulation,
  simulateNext,
  World (..),
  Environment (..),
  Animal (..),
  getMap
) where

import World

newSimulation :: Int -> Int -> Int -> World (Environment Animal)
newSimulation size time numAnimals = makeWorld (makeMap size numAnimals) time

getEnv :: World (Environment Animal) -> Environment Animal
getEnv (World env) = env

simulateDayHelper :: Environment Animal -> World (Environment Animal)
simulateDayHelper env = simulateDay (World env)

simulateNext :: World (Environment Animal) -> World (Environment Animal)
simulateNext = simulateDay

