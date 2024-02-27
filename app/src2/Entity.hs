module Entity where

import Pos

class Entity e where  
  eat       :: e -> e -> e
  drink     :: e -> e -> e
  mate      :: e -> e -> e

  age       :: e -> e
  die       :: e -> e
  
  getTarget :: e -> [[Pos]] -> Pos
  
  {-
  getHunger :: e -> Double
  getThirst :: e -> Double
  getUrge   :: e -> Double
  getSex    :: e -> Bool
  getSpeed  :: e -> Int
  getAge    :: e -> Int
  getFOV    :: e -> Int
  getPos    :: e -> Pos
  

  isDead    :: e -> Bool
  -}
