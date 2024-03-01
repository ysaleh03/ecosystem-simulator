-- Entity class for project

-- Each entity data type will have:
-- 1. Food meter
-- 2. Water meter
-- 3. Reproductive meter
-- 4. Sensing radius
-- 5. Speed
-- 6. Sex

-- Class functions include:
-- 1. Eating
--    1.1 Find food
-- 2. Drinking
--    2.1 Find water
-- 3. Reproducing
--    3.1 Find mate
-- 4. Moving
--    4.1 Sensing (abstract finding function)

module Entity (
  Entity (..),
  World (..),
  PosOrd (..),
  Environment (..),
  DefaultAnimal (..),
  Animal (..),
  Resource (..),
  Want (..),
  Map (..),
  makeEmptyMap,
  Pos (..)
) where

import Map
import System.Random

type PosOrd = (Pos, Int)
data Environment a = Environment Int [PosOrd] (Map a) StdGen
  deriving (Show)

data World e = World e
    deriving (Show)
      
-- applying functions to World wrapper
instance Functor World where
  fmap fn (World world) = World (fn world)
--         
-- applying wrapped functions to World wrapper
instance Applicative World where
  pure = World
  --(<*>) (World fn) (World world) = World (fn world)
  (<*>) (World fn) = fmap fn
--                           
-- applying sequence of functions to World wrapper
instance Monad World where
  (>>=) (World world) fn = fn world

data Want = Food | Drink | Mate
  deriving (Show)

-- action functions take in a World and something else, and return an updated World
-- To see how each function is implemented and what it does, see EntityFunctions.hs
class Entity e where 
    eat   :: World (Environment (Either Animal Resource)) -> e -> World (Environment (Either Animal Resource)) --
    drink :: World (Environment (Either Animal Resource)) -> e -> World (Environment (Either Animal Resource)) --
    mate  :: World (Environment (Either Animal Resource)) -> e -> World (Environment (Either Animal Resource)) --
    move  :: World (Environment (Either Animal Resource)) -> e -> Pos -> World (Environment (Either Animal Resource)) --
    getAction :: World (Environment (Either Animal Resource)) -> e -> World (Environment (Either Animal Resource)) --
    wander :: World (Environment (Either Animal Resource)) -> e -> World (Environment (Either Animal Resource))
    findWant  :: World (Environment (Either Animal Resource)) -> e -> Want -> [Either Animal Resource] --
    getHunger :: e -> Double --
    getThirst :: e -> Double --
    getUrge :: e -> Double --
    getSenseRadius :: e -> Int --
    getSpeed :: e -> Int --
    getPos :: e -> (Int, Int, Int) --
    isOnWant :: e -> Want -> Map (Either Animal Resource) -> Bool --
    filterWant :: e -> [Either Animal Resource] -> Want -> [Either Animal Resource] --
    ageUp :: e -> e --
    isExpired :: e -> Bool --
    updatePos :: e -> Pos -> e --
    getPosOrd :: e -> PosOrd --
    isResource :: e -> Maybe Resource --
    isAnimal :: e -> Maybe Animal --
    die :: e -> e --
    

-- DefaultAnimal parameters are listed below in order:
-- hunger (death upon dropping below 0)
-- thirst (death upon dropping below 0)
-- reproductive urge (no death upon dropping below 0, but will often not do anything until reproductive urge can be filled)
-- sensing radius (Manhattan Distance in which the animal can detect entities)
-- speed (speed of the animal: turn order and Manhattan Distance it can move in one turn)
-- Sex (True for Male, False for Female, although this is arbitrary as the only requirement for most animals is that the sex is opposite for reproduction)
-- Position (x,y,z coordinates on the map, z is an indicator of its "depth" as not an actual position, but rather a way to specify which animal at the (x,y) position we want)
-- Lifetime (after this period, the animal dies regardless of its meters)
-- base max hunger
-- base max thirst
-- mate max urge
-- hunger tick modifier
-- thirst tick modifier
-- urge tick modifier
-- max Lifetime
data DefaultAnimal  = DefaultAnimal Double Double Double Int Int Bool Pos Int Double Double Double Double Double Double Int
  deriving (Show)
data Animal = Fox DefaultAnimal | Rabbit DefaultAnimal
  deriving (Show)
-- Resource parameters are listed below in order:
-- amount of resource (grass can regen, but water is finite, though often near unlimited so it doesn't really matter)
-- position of resource (same behaviour as DefaultAnimal position)
data Resource = Grass Double Pos | Water Double Pos
  deriving (Show)
