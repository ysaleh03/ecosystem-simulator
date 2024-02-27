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
  randomGrid3D,
  merge3DArrays,
  makeEmptyMap,
  Pos (..)
) where

import Map

type PosOrd = (Pos, Int)
data Environment a = Environment Int [PosOrd] (Map a)
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

class Entity e where 
    -- if successful, return True otherwise False
    eat   :: World (Environment e) -> e -> World (Environment e) --
    drink :: World (Environment e) -> e -> World (Environment e) --
    mate  :: World (Environment e) -> e -> World (Environment e) --
    findWant  :: World (Environment e) -> e -> Want -> [e] --
    move  :: World (Environment e) -> e -> Pos -> World (Environment e) --
    getHunger :: e -> Double --
    getThirst :: e -> Double --
    getUrge :: e -> Double --
    getSenseRadius :: e -> Int --
    getSpeed :: e -> Int --
    getPos :: e -> (Int, Int, Int) --
    getAction :: World (Environment e) -> e -> World (Environment e) --
    isOnWant :: e -> Want -> Map e -> Bool --
    filterWant :: e -> [e] -> Want -> [e] --
    wander :: World (Environment e) -> e -> World (Environment e)
    ageUp :: e -> e --
    isExpired :: e -> Bool --
    updatePos :: e -> Pos -> e --
    getPosOrd :: e -> PosOrd --
    isResource :: e -> Maybe Resource --
    isAnimal :: e -> Maybe Animal --
    die :: e -> e --
    

-- hunger
-- thirst
-- reproductive urge
-- sensing radius
-- speed
-- Sex
-- Position
-- Lifetime
data DefaultAnimal  = DefaultAnimal Double Double Double Int Int Bool Pos Int
  deriving (Show)
data Animal = Fox DefaultAnimal | Rabbit DefaultAnimal
  deriving (Show)
-- Resources: Grass and Water
-- amt of resource
-- position of resource
data Resource = Grass Double Pos | Water Double Pos
  deriving (Show)
