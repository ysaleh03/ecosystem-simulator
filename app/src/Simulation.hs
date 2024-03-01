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

newSimulation :: Int -> Int -> Int -> StdGen -> World (Environment (Either Animal Resource))
newSimulation size time numEntities gen = makeWorld (merge3DArrays animalMap resourceMap) time gen2
  where
    (animalMap, gen1) = makeAnimalMap size numEntities gen
    (resourceMap, gen2) = makeResourceMap size (5*numEntities) gen1

getEnv :: World (Environment (Either Animal Resource)) -> Environment (Either Animal Resource)
getEnv (World env) = env

simulateDayHelper :: Environment (Either Animal Resource) -> World (Environment (Either Animal Resource))
simulateDayHelper env = simulateDay (World env)

simulateNext :: World (Environment (Either Animal Resource)) -> World (Environment (Either Animal Resource))
simulateNext wrld@(World (Environment _ _ m _))= trace (worldInfo wrld ++ "\n") ((simulateDay wrld) >>= updateTimer)

