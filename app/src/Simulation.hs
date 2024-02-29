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
import Debug.Trace
import System.Random

newSimulation :: Int -> Int -> Int -> World (Environment (Either Animal Resource))
newSimulation size time numEntities = trace (show mp) wrld
  where
    gen = mkStdGen 23845707617183745609563472
    (animalMap, gen1) = makeAnimalMap size numEntities gen
    (resourceMap, gen2) = makeResourceMap size (5*numEntities) gen1
    wrld@(World (Environment _ _ mp)) = makeWorld (merge3DArrays animalMap resourceMap) time

getEnv :: World (Environment (Either Animal Resource)) -> Environment (Either Animal Resource)
getEnv (World env) = env

simulateDayHelper :: Environment (Either Animal Resource) -> World (Environment (Either Animal Resource))
simulateDayHelper env = simulateDay (World env)

simulateNext :: World (Environment (Either Animal Resource)) -> World (Environment (Either Animal Resource))
simulateNext = simulateDay

