module World (
  World (..),
  Entity(..),
  Animal(..),
  Resource(..),
  Map (..),
  Environment(..),
  makeWorld,
  makeMap,
  getMap,
  simulateDay
) where

import Data.List
import Data.List.Extras.Argmax
import EntityFunctions
import System.Random
--import Init3DGridFunction


envInfo :: Environment e -> String
envInfo (Environment time orderedPositions map) =
  "Time: "++show time++" Entities: "++show (length orderedPositions)++" Map Size: "++show (length map)

worldInfo :: World (Environment e) -> String
worldInfo (World (Environment time poss map)) = envInfo (Environment time poss map)

-- Insert an element into a sorted list based on a comparison function
insertToSorted :: a -> [a] -> (a -> a -> Bool) -> [a]
insertToSorted e [] _ = [e]
insertToSorted e (h:t) comp
  | comp e h = e:(h:t)
  | otherwise = h:(insertToSorted e t comp)

-- makeWorld

makeRandomResource :: RandomGen g => Int -> g -> (Resource, g)
makeRandomResource n gen =
  if binaryResource then
    (Grass startVal (xPos, yPos, n), gen4)
  else
    -- arbitrary number yipeeee
    (Water 1000 (xPos, yPos, n), gen4)
  where
    (startVal,gen1) = randomR (((fromIntegral n)/10.0)+1.0,fromIntegral n) gen
    (xPos,gen2) = randomR (0,n-1) gen1
    (yPos,gen3) = randomR (0,n-1) gen2
    (binaryResource, gen4) = randomR (True,False) gen3
  
makeRandomAnimal :: RandomGen g => Int -> g -> (Animal, g)
makeRandomAnimal n gen =
  if binaryAnimal then
    (Fox (DefaultAnimal 10.0 10.0 10.0 senseR speed sex (xPos, yPos, n) 6), gen6)
  else
    (Rabbit (DefaultAnimal 10.0 10.0 10.0 senseR speed sex (xPos, yPos, n) 6), gen6)
  where
    (senseR,gen1) = randomR (2,5) gen
    (speed,gen2) = randomR (1,5) gen1
    (sex,gen3) = randomR (True,False) gen2
    (xPos,gen4) = randomR  (0,n-1) gen3
    (yPos,gen5) = randomR (0,n-1) gen4
    (binaryAnimal, gen6) = randomR (True,False) gen5

makeMap :: Int -> Int -> Map Animal
makeMap size numAnimals = foldr replaceEntityAt (makeEmptyMap size) (fst $ unzip (foldr (\ _ ((a, g):t) -> (makeRandomAnimal size g):((a,g):t)) [makeRandomAnimal size (mkStdGen 3)] [0..numAnimals])) 

makeWorld :: Entity e => Map e -> Int -> World (Environment e)
makeWorld map time = World (Environment time (foldr (\x y -> insertToSorted x y (\ (_,g) (_,h) -> g > h)) [] [getPosOrd z | x<-map, y<-x, z<-y]) map)

-- execute action updates the world by querying the action the animal wants to take,
-- and updating the world state based on that action
executeAction :: Entity e => PosOrd -> Environment e -> World (Environment e)
executeAction ((x, y, z), _) env@(Environment time poss map)  = 
  let entity = map !! x !! y !! z in getAction (World env) entity

decrementTimer :: Environment e -> World (Environment e)
decrementTimer (Environment time poss map) = World (Environment (time - 1) poss map)

getTimer :: Environment e -> Int
getTimer (Environment time _ _) = time

getMap :: World (Environment e) -> Map e
getMap (World (Environment _ _ map)) = map

updateEntityList :: Entity e => Environment e -> World (Environment e)
updateEntityList (Environment time poss map) = 
  World (Environment time newPosOrds updatedMap)
  where
  -- first, remove all expired entities from map
    newMap = [[[ageUp entity | entity<-col, not $ isExpired entity] | col<-row] | row<-map]
  -- update all the positions with new entity positions
    updatedMap = [[[updatePos entity (x,y,z) | (z,entity)<-(zip [0..length col] col)] | (y,col)<-(zip [0..length row] row)] | (x,row)<-(zip [0..length newMap] newMap)]
  -- finally, traverse the list of entities and insert them in the proper order based on speed to [PosOrd]
    flattenedList = concat $ concat updatedMap
    newPosOrds = foldr (\x y -> let posOrd = (getPos x, getSpeed x) in insertToSorted posOrd y (\(_,speed1) (_,speed2) -> speed1 > speed2)) [] flattenedList

simulateDay :: Entity e => World (Environment e) -> World (Environment e)
--simulateDay (World (Environment 0 entityPositions map)) = return (Environment 0 entityPositions map)
--simulateDay (World (Environment time [] map)) = return (Environment time [] map)
simulateDay world@(World env@(Environment time entityPositions map)) = 
    -- for each entity, carry out an action
--    newEnv <- foldl (\currWorld posOrd -> currWorld >>= executeAction posOrd) world entityPositions
--    newEnv <- foldl (\currWorld@(World (Environment ctime cposs cmap)) _ -> if null cposs then currWorld else currWorld >>= executeAction (head cposs)) world [0..(length entityPositions)]
    if null entityPositions then
      do
      newEnv <- updateEntityList env
      return newEnv
    else
      do
      newEnv <- simulateDay (executeAction (head entityPositions) env)
      -- execute the decision and update the world state
      -- Update the timer (decrement by one)
      --newEnv2 <- decrementTimer newEnv
      -- Update the entity list according to the map (read all entities and add them)
      --    also, remove any whose lifespans expired
      --newWorld <- updateEntityList newEnv

      return newEnv
    {-
    if (getTimer newEnv) > 0 then
      simulateDay (World newEnv)
    else
      World newEnv
      -}
