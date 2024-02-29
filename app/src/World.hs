module World (
  World (..),
  Entity(..),
  Animal(..),
  Resource(..),
  Map (..),
  Environment(..),
  makeWorld,
  makeAnimalMap,
  makeResourceMap,
  merge3DArrays,
  getMap,
  simulateDay
) where

import Data.List
import Data.List.Extras.Argmax
import EntityFunctions
import System.Random
--import Init3DGridFunction

merge3DArrays :: (Entity e) => [[[e]]] -> [[[e]]] -> [[[e]]]
merge3DArrays [] [] = []
merge3DArrays (x:xs) (y:ys) = merge2DArrays x y : merge3DArrays xs ys
merge3DArrays _ _ = error "Arrays must have the same dimensions"

merge2DArrays :: (Entity e) => [[e]] -> [[e]] -> [[e]]
merge2DArrays [] [] = []
merge2DArrays (x:xs) (y:ys) = (updateAllZCoords (mergeRows x y)) : merge2DArrays xs ys
merge2DArrays _ _ = error "Arrays must have the same dimensions"

mergeRows :: (Entity e) => [e] -> [e] -> [e]
mergeRows [] [] = []
mergeRows (x:xs) (y:ys) = mergeAnimals x y ++ mergeRows xs ys
mergeRows x [] = x
mergeRows [] y = y

mergeAnimals :: (Entity e) => e -> e -> [e]
mergeAnimals entity1 entity2 = [entity1, entity2]

updateAllZCoords :: (Entity e) => [e] -> [e]
updateAllZCoords entityLst =
  let (x, y) = getXY (head entityLst) -- Extract x and y from the first entity
    in [updatePos e (x, y, z) | (e, z) <- zip entityLst [0 ..]] -- Update z for each entity

getXY :: (Entity e) => e -> (Int, Int)
getXY anEntity = (getFirst (getPos anEntity), getSecond (getPos anEntity))

getFirst :: (a, b, c) -> a
getFirst (x, _, _) = x

getSecond :: (a, b, c) -> b
getSecond (_, y, _) = y

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

makeAnimalMap :: RandomGen g => Int -> Int -> g -> (Map (Either Animal Resource), g)
makeAnimalMap size numAnimals gen = (foldr replaceEntityAt (makeEmptyMap size) (map (Left) (fst $ pairs)), head $ snd pairs)
  where
    pairs = unzip (foldr (\ _ ((a, g):t) -> (makeRandomAnimal size g):((a,g):t)) [makeRandomAnimal size gen] [0..numAnimals]) 

makeResourceMap :: RandomGen g => Int -> Int -> g -> (Map (Either Animal Resource), g)
makeResourceMap size numResources gen = (foldr replaceEntityAt (makeEmptyMap size) (map (Right) (fst $ pairs)), head $ snd pairs)
  where
    pairs = unzip (foldr (\ _ ((a, g):t) -> (makeRandomResource size g):((a,g):t)) [makeRandomResource size gen] [0..numResources]) 

makeWorld :: Entity e => Map e -> Int -> World (Environment e)
makeWorld map time = World (Environment time (foldr (\x y -> insertToSorted x y (\ (_,g) (_,h) -> g > h)) [] [getPosOrd z | x<-map, y<-x, z<-y]) map)

-- execute action updates the world by querying the action the animal wants to take,
-- and updating the world state based on that action
executeAction :: PosOrd -> Environment (Either Animal Resource) -> World (Environment (Either Animal Resource))
executeAction ((x, y, z), _) env@(Environment time poss map)  = 
  let entity = map !! x !! y !! z in getAction (World env) entity

decrementTimer :: Environment e -> World (Environment e)
decrementTimer (Environment time poss map) = World (Environment (time - 1) poss map)

getTimer :: Environment e -> Int
getTimer (Environment time _ _) = time

getMap :: World (Environment e) -> Map e
getMap (World (Environment _ _ map)) = map

updateEntityList :: Environment (Either Animal Resource) -> World (Environment (Either Animal Resource))
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

simulateDay :: World (Environment (Either Animal Resource)) -> World (Environment (Either Animal Resource))
--simulateDay (World (Environment 0 entityPositions map)) = return (Environment 0 entityPositions map)
--simulateDay (World (Environment time [] map)) = return (Environment time [] map)
simulateDay world@(World env@(Environment time entityPositions map)) = 
    -- for each entity, carry out an action
    if null entityPositions then
      updateEntityList env
    else
      simulateDay (executeAction (head entityPositions) env)
