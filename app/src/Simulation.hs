module Simulation (
  newSimulation,
  simulateNext,
  World (..),
  Environment (..),
  Animal (..),
  Resource (..),
  getMap
) where

import World

newSimulation :: Int -> Int -> Int -> World (Environment (Either Animal Resource))
newSimulation size time numEntities = makeWorld (merge3DArrays (makeAnimalMap size numEntities) (makeResourceMap size numEntities)) time

getEnv :: World (Environment (Either Animal Resource)) -> Environment (Either Animal Resource)
getEnv (World env) = env

simulateDayHelper :: Environment (Either Animal Resource) -> World (Environment (Either Animal Resource))
simulateDayHelper env = simulateDay (World env)

simulateNext :: World (Environment (Either Animal Resource)) -> World (Environment (Either Animal Resource))
simulateNext = simulateDay

