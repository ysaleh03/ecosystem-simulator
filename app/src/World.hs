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
  simulateDay,
  updateTimer,
  worldInfo,
  envInfo
) where

import Data.List
import Data.List.Extras.Argmax
import EntityFunctions
import System.Random
import Data.Either

-- takes two 3D arrays of the same type, and merges them such that the innermost arrays are appended to each other at corresponding x and y positions (first and second indices)
merge3DArrays :: (Entity e) => [[[e]]] -> [[[e]]] -> [[[e]]]
merge3DArrays [] [] = []
merge3DArrays (x:xs) (y:ys) = merge2DArrays x y : merge3DArrays xs ys
merge3DArrays _ _ = error "Arrays must have the same dimensions"

{- HELPERS FOR merge3DArrays -}

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

{- END HELPERS FOR merge3DArrays -}

-- some info functions helpful for debugging and getting simple information from the Environment
envInfo :: Environment (Either Animal Resource) -> String
envInfo (Environment time orderedPositions worldMap gen) =
  "Time: "++show time++" Entities: "++show (length orderedPositions)++" Map Size: "++show (length worldMap) ++" Foxes: "++show nFoxes++" Rabbits: "++show nRabbits++" Grass: "++show nGrass++" Water: "++show nWater
  where
    nFoxes = length [a | a<-concat $ concat worldMap, isLeft a, isFox a]
    nRabbits = length [a | a<-concat $ concat worldMap, isLeft a, not $ isFox a]
    nGrass = length [a | a<-concat $ concat worldMap, isRight a, isGrass a]
    nWater = length [a | a<-concat $ concat worldMap, isRight a, not $ isGrass a]
    isFox (Left (Fox _)) = True
    isFox _ = False
    isGrass (Right (Grass _ _)) = True
    isGrass _ = False

worldInfo :: World (Environment (Either Animal Resource)) -> String
worldInfo (World (Environment time poss map gen)) = envInfo (Environment time poss map gen)

-- Insert an element into a sorted list based on a comparison function
insertToSorted :: a -> [a] -> (a -> a -> Bool) -> [a]
insertToSorted e [] _ = [e]
insertToSorted e (h:t) comp
  | comp e h = e:(h:t)
  | otherwise = h:(insertToSorted e t comp)

-- given the size of the map (assuming dim x == dim y of the map) and some generator, return a tuple of some random Resource and the new generator
makeRandomResource :: RandomGen g => Int -> g -> (Resource, g)
makeRandomResource n gen =
  if whichResource == 0 then
    (Grass startVal (xPos, yPos, n), gen4)
  else
    -- arbitrary number yipeeee
    (Water 1000.0 (xPos, yPos, n), gen4)
  where
    -- get random values for the resources
    (startVal,gen1) = randomR (((fromIntegral n)/10.0)+1.0,fromIntegral n) gen
    (xPos,gen2) = randomR (0,n-1) gen1
    (yPos,gen3) = randomR (0,n-1) gen2
    -- equal distribution, should be roughly 1:1 water to grass ratio
    (whichResource, gen4) = uniformR (0 :: Int,1 :: Int) gen3
  
-- given the size of the map (assuming dim x == dim y of the map) and some generator, return a tuple of some random Animal and the new generator
makeRandomAnimal :: RandomGen g => Int -> g -> (Animal, g)
makeRandomAnimal n gen =
  if whichAnimal == 0 then
    (Fox (DefaultAnimal 10.0 10.0 10.0 (senseR + 1) (speed + 1) sex (xPos, yPos, n) 15 10.0 10.0 15.0 1.0 1.0 1.0 15), gen6)
  else
    (Rabbit (DefaultAnimal 10.0 10.0 10.0 senseR speed sex (xPos, yPos, n) 7 10.0 10.0 12.0 0.5 1.0 1.0 7), gen6)
  where
    -- get random values for the animals
    (senseR,gen1) = randomR (2,5) gen
    (speed,gen2) = randomR (1,5) gen1
    (sex,gen3) = randomR (True,False) gen2
    (xPos,gen4) = randomR  (0,n-1) gen3
    (yPos,gen5) = randomR (0,n-1) gen4
    -- not equal distribution, the ratio of Rabbits to Foxes should be
    --  ~5:1
    --    can be changed though, by just changing the second parameter in the tuple passed below. The ratio of Rabbits to Foxes would then be
    --    ~k:1
    --    assuming the tuple is (0,k)
    (whichAnimal, gen6) = uniformR (0 :: Int,7 :: Int) gen5

-- Given the size of the map (size), the number of animals to generate (numAnimals), and a random generator (gen), returns a randomly generated map of exactly numAnimals animals consisting of ONLY Animals (still of the Either type, however), and a new random generator.
--    it accomplishes this by repeatedly inserting randomly generated animals to a map until the desired amount is reached
makeAnimalMap :: RandomGen g => Int -> Int -> g -> (Map (Either Animal Resource), g)
makeAnimalMap size numAnimals gen = (foldr replaceEntityAt (makeEmptyMap size) (map (Left) (fst $ pairs)), head $ snd pairs)
  where
    pairs = unzip (foldr (\ _ ((a, g):t) -> (makeRandomAnimal size g):((a,g):t)) [makeRandomAnimal size gen] [0..numAnimals]) 

-- Given the size of the map (size), the number of resources to generate (numResources), and a random generator (gen), returns a randomly generated map of exactly numResources resources consisting of ONLY Resources (still of the Either type, however), and a new random generator.
--    it accomplishes this by repeatedly inserting randomly generated resources to a map until the desired amount is reached
makeResourceMap :: RandomGen g => Int -> Int -> g -> (Map (Either Animal Resource), g)
makeResourceMap size numResources gen = (foldr replaceEntityAt (makeEmptyMap size) (map (Right) (fst $ pairs)), head $ snd pairs)
  where
    pairs = unzip (foldr (\ _ ((a, g):t) -> (makeRandomResource size g):((a,g):t)) [makeRandomResource size gen] [0..numResources]) 

-- Given a map and some initial time value, encapsulates the map and time to the World Monad and initializes the proper values for the turn list
makeWorld :: Entity e => Map e -> Int -> StdGen -> World (Environment e)
makeWorld map time gen = World (Environment time (foldr (\x y -> insertToSorted x y (\ (_,g) (_,h) -> g > h)) [] [getPosOrd z | x<-map, y<-x, z<-y]) map gen)

-- execute action updates the world by querying the action the animal wants to take, and updating the world state based on that action
executeAction :: PosOrd -> Environment (Either Animal Resource) -> World (Environment (Either Animal Resource))
executeAction ((x, y, z), _) env@(Environment time poss map gen)  = 
  let entity = map !! x !! y !! z in getAction (World env) entity

-- adds 1 to the timer of the environment and encapsulates it to a world
updateTimer :: Environment e -> World (Environment e)
updateTimer (Environment time poss map gen) = World (Environment (time + 1) poss map gen)

-- gets the timer from an environment
getTimer :: Environment e -> Int
getTimer (Environment time _ _ _) = time

-- gets the map from a world
getMap :: World (Environment e) -> Map e
getMap (World (Environment _ _ map _)) = map

-- updates the turn order of entities and removes all "expired" entities
--  it first updates the map by removing expired entities, updates the z-coordinates of all the entities.
--  Then, it collects all the entities in the map, and creates a sorted turn order based on entity speed
updateEntityList :: Environment (Either Animal Resource) -> World (Environment (Either Animal Resource))
updateEntityList (Environment time poss map gen) = 
  World (Environment time newPosOrds updatedMap gen)
  where
  -- first, remove all expired entities from map
    newMap = [[[ageUp entity | entity<-col, not $ isExpired entity] | col<-row] | row<-map]
  -- update all the positions with new entity positions
    updatedMap = [[[updatePos entity (x,y,z) | (z,entity)<-(zip [0..length col] col)] | (y,col)<-(zip [0..length row] row)] | (x,row)<-(zip [0..length newMap] newMap)]
  -- finally, traverse the list of entities and insert them in the proper order based on speed to [PosOrd]
    flattenedList = concat $ concat updatedMap
    newPosOrds = foldr (\x y -> let posOrd = (getPos x, getSpeed x) in insertToSorted posOrd y (\(_,speed1) (_,speed2) -> speed1 > speed2)) [] flattenedList

-- simulates a "day" or "cycle" given a World.
--  returns an updated world after every entity has "taken its turn" by repeatedly querying entities and updating the map based on their choices
simulateDay :: World (Environment (Either Animal Resource)) -> World (Environment (Either Animal Resource))
simulateDay world@(World env@(Environment time entityPositions map gen)) = 
    -- for each entity, carry out an action
    if null entityPositions then
      -- if there are no more entities to call, return the updated map and turn order for the next "day"/"cycle"
      updateEntityList env
    else
      -- otherwise, execute the action for that entity, and query the next one in the turn order
      simulateDay (executeAction (head entityPositions) env)
