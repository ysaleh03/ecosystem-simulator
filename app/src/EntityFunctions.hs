module EntityFunctions (
  Entity (..),
  World (..),
  PosOrd (..),
  Environment (..),
  DefaultAnimal (..),
  Animal (..),
  Resource (..),
  Want (..),
  Map (..),
  Pos (..),
  makeEmptyMap,
  replaceEntityAt
) where

import Entity
import Data.List.Extras.Argmax
import Data.List
import Data.Maybe
import System.Random
import Debug.Trace

-- get the position and speed pair from an entity
getPosOrdEntity :: Entity a => a -> PosOrd
getPosOrdEntity entity = (getPos entity, getSpeed entity)

-- get the distance between an entity and some position (ignores the speed of the PosOrd pair) on the x-y plane
distance :: Entity e => (e, PosOrd) -> Int
distance (e, ((x1,y1,_), _)) = 
  let ((x2,y2,_), _) = getPosOrd e in (abs (x2 - x1)) + (abs (y2 - y1))

-- function that clamps a value based on a min-max tuple
clamp (min,max) val 
  | val > max = max
  | val < min = min
  | otherwise = val

{- MAP HELPERS -}

-- this was useful for splitting list https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
-- replaceEntityAt takes in some entity and a map of entities, and replaces the entity at the given position.
--  if the z value is greater than or equal to the length of the list (map !! x !! y), then the z value of the entity is changed to the length of the list at the entity's (x,y) position.
replaceEntityAt :: Entity e => e -> Map e -> Map e
replaceEntityAt entity worldMap = 
  newMap
  where
    (x, y, z) = getPos entity
    entities = worldMap !! x !! y
    (frontInner, tempRearInner) = splitAt z entities
    rearInner = if null tempRearInner then tempRearInner else tail tempRearInner 
    -- if the z value of the entity is greater than the list's length, then we set it's Z value to the length of the list
    newEntities = if null tempRearInner then frontInner ++ (updatePos entity (x,y,length frontInner)) : rearInner else frontInner ++ entity : rearInner
    (frontMiddle, tempRearMiddle) = splitAt y (worldMap !! x)
    rearMiddle = if null tempRearMiddle then tempRearMiddle else tail tempRearMiddle
    newY = frontMiddle ++ newEntities : rearMiddle
    (frontOuter, tempRearOuter) = splitAt x worldMap
    rearOuter = if null tempRearOuter then tempRearOuter else tail tempRearOuter
    newMap = frontOuter ++ newY : rearOuter

{- END MAP HELPERS -}

{- DEFAULTANIMAL HELPERS-}

-- the function that is called when a "day/cycle" passes. Decreases their lifetime and their urges
ageUpDefaultAnimal :: DefaultAnimal -> DefaultAnimal
ageUpDefaultAnimal (DefaultAnimal hunger thirst urge senseR speed sex pos lifetime) =
  DefaultAnimal (hunger-1.0) (thirst-1.0) (urge-1.0) senseR speed sex pos (lifetime - 1)

-- returns a copy of the DefaultAnimal with an updated position
updatePosDefaultAnimal :: DefaultAnimal -> Pos -> DefaultAnimal
updatePosDefaultAnimal (DefaultAnimal hunger thirst urge senseR speed sex _ lifetime) pos =
  DefaultAnimal hunger thirst urge senseR speed sex pos lifetime

-- gets the hunger of a DefaultAnimal
getHungerDefaultAnimal :: DefaultAnimal -> Double
getHungerDefaultAnimal (DefaultAnimal hunger _ _ _ _ _ _ _) = hunger

-- gets the thirst of a DefaultAnimal
getThirstDefaultAnimal :: DefaultAnimal -> Double
getThirstDefaultAnimal (DefaultAnimal _ thirst _ _ _ _ _ _) = thirst

-- gets the urge of a DefaultAnimal
getUrgeDefaultAnimal :: DefaultAnimal -> Double
getUrgeDefaultAnimal (DefaultAnimal _ _ urge _ _ _ _ _) = urge

-- gets the sense radius of a DefaultAnimal
getSenseRadiusDefaultAnimal :: DefaultAnimal -> Int
getSenseRadiusDefaultAnimal (DefaultAnimal _ _ _ senseR _ _ _ _) = senseR

-- for eat, drink, mate, etc. 
-- Maybe we should have values for satiated (max amount they can store instead of 10.0)
--  and also values for how much the meters decrement by each day?
--  brings the eat meter to its max value (animal is satiated)
eatDefaultAnimal :: DefaultAnimal -> DefaultAnimal
eatDefaultAnimal (DefaultAnimal _ thirst urge senseR speed sex pos lifetime) =
  DefaultAnimal 10.0 thirst urge senseR speed sex pos lifetime

-- brings the drink meter to its max value (animal is quenched)
drinkDefaultAnimal :: DefaultAnimal -> DefaultAnimal
drinkDefaultAnimal (DefaultAnimal hunger _ urge senseR speed sex pos lifetime) =
  DefaultAnimal hunger 10.0 urge senseR speed sex pos lifetime

-- brings the urge meter to its max value (animal is satisfied)
mateDefaultAnimal :: DefaultAnimal -> DefaultAnimal
mateDefaultAnimal (DefaultAnimal hunger thirst _ senseR speed sex pos lifetime) =
  DefaultAnimal hunger thirst 10.0 senseR speed sex pos lifetime

{- END DEFAULTANIMAL HELPERS -}

{- ANIMAL HELPERS -}

-- Helper function for making sure foxes and rabbits can only mate with foxes and rabbits of the opposite gender, respectively.
checkCanMate :: Animal -> Animal -> Bool
checkCanMate (Fox (DefaultAnimal _ _ _ _ _ sex1 (x1,y1,_) _)) (Fox (DefaultAnimal _ _ _ _ _ sex2 (x2,y2,_) _)) =
  sex1 /= sex2 && (x1,y1) == (x2,y2)
checkCanMate (Rabbit (DefaultAnimal _ _ _ _ _ sex1 (x1,y1,_) _)) (Rabbit (DefaultAnimal _ _ _ _ _ sex2 (x2,y2,_) _)) =
  sex1 /= sex2 && (x1,y1) == (x2,y2)
checkCanMate _ _ = False

-- helper for returning the DefaultAnimal of an Animal
getDefaultAnimal :: Animal -> DefaultAnimal
getDefaultAnimal (Fox da) = da
getDefaultAnimal (Rabbit da) = da

-- creates a new Animal with inherited traits at the position of the parents, given two parents
createBabyAnimal :: World (Environment (Either Animal Resource)) -> Animal -> Animal -> World (Environment (Either Animal Resource))
createBabyAnimal (World (Environment time poss worldMap gen)) (Fox (DefaultAnimal h1 t1 u1 r1 s1 x1 (p11,p12,p13) l1)) (Fox (DefaultAnimal h2 t2 u2 r2 s2 x2 (p21,p22,p23) l2))
  | u1 > u2 = 
    let babyAnimal = Fox (DefaultAnimal 10.0 10.0 15.0 r1 s2 x1 (p11,p12,length (worldMap !! p11 !! p12)) 10)
        newMap = replaceEntityAt (Left babyAnimal) worldMap
    in World (Environment time poss newMap gen)
    
  | otherwise = 
    let babyAnimal = Fox (DefaultAnimal 10.0 10.0 15.0 r2 s1 x2 (p11,p12,length (worldMap !! p11 !! p12)) 10)
        newMap = replaceEntityAt (Left babyAnimal) worldMap
    in World (Environment time poss newMap gen)
createBabyAnimal (World (Environment time poss worldMap gen)) (Rabbit (DefaultAnimal h1 t1 u1 r1 s1 x1 (p11,p12,p13) l1)) (Rabbit (DefaultAnimal h2 t2 u2 r2 s2 x2 (p21,p22,p23) l2))
  | u1 > u2 = 
    let babyAnimal = Rabbit (DefaultAnimal 10.0 10.0 15.0 r1 s2 x1 (p11,p12,length (worldMap !! p11 !! p12)) 10)
        newMap = replaceEntityAt (Left babyAnimal) worldMap
    in World (Environment time poss newMap gen)
    
  | otherwise = 
    let babyAnimal = Rabbit (DefaultAnimal 10.0 10.0 15.0 r2 s1 x2 (p11,p12,length (worldMap !! p11 !! p12)) 10)
        newMap = replaceEntityAt (Left babyAnimal) worldMap
    in World (Environment time poss newMap gen)
createBabyAnimal world _ _ = world

{- END ANIMAL HELPERS -}

-- we define all the functions so that Animal can be an instance of Entity
instance Entity Animal where
  -- this is used since we do not remove them until the end of the day cycle
  -- An animal is slated to die, since the lifetime of an animal only decreases, setting it to a value below 0 should remove it from the map on the next iteration.
  --    ie. marking it for removal
  die (Fox (DefaultAnimal hunger thirst urge senseR speed sex pos lifetime)) =
    Fox (DefaultAnimal hunger thirst urge senseR speed sex pos (-1))
  die (Rabbit (DefaultAnimal hunger thirst urge senseR speed sex pos lifetime)) =
    Rabbit (DefaultAnimal hunger thirst urge senseR speed sex pos (-1))

  -- animal wanders around the map, since it cannot find the want it is searching for
  -- computes a random position to move to, and then calls move with that position.
  -- returns the updated world after having moved the animal
  wander world@(World (Environment _ _ worldMap gen)) animal =
    --move world animal (srcX+changeInX,srcY+trueChangeInY,0)
    move world animal (clampChangeInX, clampChangeInY, length (worldMap !! clampChangeInX !! clampChangeInY))
    where
      (srcX,srcY,_) = getPos animal
      speed = getSpeed animal
      (changeInX,ngen1) = uniformR (-speed,speed) gen
      changeInY = (speed - (abs changeInX)) * (fst $ uniformR (-1,1) ngen1)
      trueChangeInY = if changeInY == 0 then speed - (abs changeInX) else changeInY
      clampChangeInX = clamp (0,(length worldMap) - 1) (srcX+changeInX)
      clampChangeInY = clamp (0,(length (worldMap !! clampChangeInX)) - 1) (srcY+trueChangeInY)

  -- "ages up" the animal, by calling the constructor on the helper that updates the lifetime of the animal, and returns the updated animal
  ageUp (Fox da) = Fox $ ageUpDefaultAnimal da
  ageUp (Rabbit da) = Rabbit $ ageUpDefaultAnimal da

  -- lifetime runs out, or some meter reaches 0, we return true (animal should be dead)
  isExpired (Fox (DefaultAnimal hunger thirst urge _ _ _ _ lifetime)) =
    any (<=0.0) [hunger, thirst] || lifetime <= 0
  isExpired (Rabbit (DefaultAnimal hunger thirst urge _ _ _ _ lifetime)) =
    any (<=0.0) [hunger, thirst] || lifetime <= 0

  -- updates the position of the animal by calling the constructor on the helper that updates the position of the animal, and returns the updated animal
  updatePos (Fox da) pos = Fox $ updatePosDefaultAnimal da pos
  updatePos (Rabbit da) pos = Rabbit $ updatePosDefaultAnimal da pos

  -- returns the speed of the animal
  getSpeed (Fox (DefaultAnimal _ _ _ _ speed _ _ _)) = speed
  getSpeed (Rabbit (DefaultAnimal _ _ _ _ speed _ _ _)) = speed

  -- returns the position of the animal
  getPos (Fox (DefaultAnimal _ _ _ _ _ _ pos _)) = pos
  getPos (Rabbit (DefaultAnimal _ _ _ _ _ _ pos _)) = pos

  -- single definition should work for all animals. Alias for getPosOrdEntity, which returns a PosOrd for some entity
  getPosOrd = getPosOrdEntity

  -- calls the DefaultAnimal helper to get the hunger of the animal
  getHunger (Fox da) = getHungerDefaultAnimal da
  getHunger (Rabbit da) = getHungerDefaultAnimal da

  -- calls the DefaultAnimal helper to get the thirst of the animal
  getThirst (Fox da) = getThirstDefaultAnimal da
  getThirst (Rabbit da) = getThirstDefaultAnimal da

  -- calls the DefaultAnimal helper to get the urge of the animal
  getUrge (Fox da) = getUrgeDefaultAnimal da
  getUrge (Rabbit da) = getUrgeDefaultAnimal da

  -- calls the DefaultAnimal helper to get the sense radius of the animal
  getSenseRadius (Fox da) = getSenseRadiusDefaultAnimal da
  getSenseRadius (Rabbit da) = getSenseRadiusDefaultAnimal da

  -- returns Nothing for isResource, since it is not a resource
  isResource _ = Nothing
  -- isAnimal is an alias for Just since an Animal type is an animal
  isAnimal = Just

  -- single definition should work for all animals
  -- getAction "chooses" an action based on the animal's meters.
  --    First, it finds which need it needs to fulfill the most
  --    Then, it tries to "sense" (find) an entity it wants
  --      If it finds and entity it wants, and is on the same slot (x,y), then it calls the action and returns the updated world
  --      If it finds an entity it wants, but is not on the same slot (x,y), then it moves towards that slot (this may take multiple "turns"). returns the updated world after movement.
  --      If it does not find an entity it wants, it wanders around, and tries again on its next "turn". returns the updated world after movement.
  getAction (World (Environment time (h:poss) worldMap gen)) animal
    -- it cannot sense an item it needs
    | null items = wander world animal
    -- it senses an item, check if it's in the same square (distance only counts x and y, not z)
    | distance (animal, getPosOrd firstItem) == 0  = action world animal
    -- it senses the item, but is not on it. Go towards the item
    | otherwise = move world animal (getPos firstItem)
    where
      world = World (Environment time poss worldMap gen)
      -- find the lowest meter: what it should fill first
      ((want, action), _) = argmin snd (zip [(Food,eat),(Drink,drink),(Mate,mate)] [getHunger animal, getThirst animal, getUrge animal])
      items = findWant world animal want
      -- sort the list of items
      ordItems = sortOn distance (zip items (replicate (length items) (getPosOrd animal)))
      -- move to closest item
      firstItem = fst $ head ordItems

  -- checks whether the animal is on the Want specified given the animal, want, and map
  --  returns true if the want exists on the same grid
  --      for instance: if a Fox and Food are passed into isOnWant, the function gets the list of entities in the same slot (x,y) as the fox, and checks to see if there is a Rabbit
  isOnWant (Fox (DefaultAnimal _ _ _ _ _ _ (x,y,_) _)) Food worldMap = 
    or [containsFood $ isAnimal e | e<-entities, not $ isExpired e]
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsFood (Just (Rabbit _)) = True
      containsFood _ = False
  isOnWant (Fox (DefaultAnimal _ _ _ _ _ _ (x,y,_) _)) Drink worldMap =
    or [containsDrink $ isResource e | e<-entities, not $ isExpired e]
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsDrink (Just (Water _ _)) = True
      containsDrink _ = False
  isOnWant (Fox (DefaultAnimal _ _ _ _ _ firstSex (x,y,_) _)) Mate worldMap = 
    or [containsMate $ isAnimal e | e<-entities, not $ isExpired e] 
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsMate (Just (Fox ( DefaultAnimal _ _ _ _ _ secondSex _ _))) = secondSex /= firstSex
      containsMate _ = False

  isOnWant (Rabbit (DefaultAnimal _ _ _ _ _ _ (x,y,_) _)) Food worldMap = 
    or [containsFood $ isResource e | e<-entities, not $ isExpired e]
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsFood (Just (Grass _ _)) = True
      containsFood _ = False
  isOnWant (Rabbit (DefaultAnimal _ _ _ _ _ _ (x,y,_) _)) Drink worldMap =
    or [containsDrink $ isResource e | e<-entities, not $ isExpired e]
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsDrink (Just (Water _ _)) = True
      containsDrink _ = False
  isOnWant (Rabbit (DefaultAnimal _ _ _ _ _ firstSex (x,y,_) _)) Mate worldMap = 
    or [containsMate $ isAnimal e | e<-entities, not $ isExpired e]
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsMate (Just (Rabbit ( DefaultAnimal _ _ _ _ _ secondSex _ _))) = secondSex /= firstSex
      containsMate _ = False

  -- filterWant is similar to isOnWant, except it returns a list of entities based on the Animal and Want, given some list of entities
  --    Note, Animal and the entities passed in DO NOT have to be in the same position
  filterWant (Fox _) entities Food = 
    [e | e<-entities, containsFood $ isAnimal e, not $ isExpired e]
    where
      containsFood (Just (Rabbit _)) = True
      containsFood _ = False
  filterWant (Fox _) entities Drink = 
    [e | e<-entities,containsDrink $ isResource e, not $ isExpired e]
    where
      containsDrink (Just (Water _ _)) = True
      containsDrink _ = False
  filterWant (Fox (DefaultAnimal _ _ _ _ _ firstSex (x,y,_) _)) entities Mate = 
    [e| e<-entities, containsMate $ isAnimal e, not $ isExpired e]
    where
      containsMate (Just (Fox ( DefaultAnimal _ _ _ _ _ secondSex _ _))) = secondSex /= firstSex
      containsMate _ = False
  filterWant (Rabbit _) entities Food = 
    [e | e<-entities, containsFood $ isResource e, not $ isExpired e]
    where
      containsFood (Just (Grass _ _)) = True
      containsFood _ = False
  filterWant (Rabbit _) entities Drink = 
    [e | e<-entities,containsDrink $ isResource e, not $ isExpired e]
    where
      containsDrink (Just (Water _ _)) = True
      containsDrink _ = False
  filterWant (Rabbit (DefaultAnimal _ _ _ _ _ firstSex (x,y,_) _)) entities Mate = 
    [e| e<-entities, containsMate $ isAnimal e, not $ isExpired e]
    where
      containsMate (Just (Rabbit ( DefaultAnimal _ _ _ _ _ secondSex _ _))) = secondSex /= not firstSex
      containsMate _ = False

  -- eat is only called if the food is found in the same slot (x,y) as the animal that is calling it!
  -- So, we pass in the world and animal, and return the world with the first valid food item "eaten" and the animal satiated
  eat (World (Environment time poss worldMap gen)) (Fox da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) =
    World (Environment time poss newMap gen)
    where
      satiatedAnimal = Left $ Fox $ eatDefaultAnimal da
      entities = worldMap !! x !! y
      newFood = die $ findFirstFood entities 
      -- should never reach the empty list case, since we check beforehand that a food item exists
      findFirstFood (entity:entities) = if isFood $ isAnimal entity then entity else findFirstFood entities
      findFirstFood [] = Left (Rabbit (DefaultAnimal 10.0 10.0 10.0 2 2 True (x,y,0) (-1)))
      isFood (Just rabbit@(Rabbit _)) = not $ isExpired rabbit
      isFood _ = False
      newMap = replaceEntityAt satiatedAnimal (replaceEntityAt newFood worldMap)
  eat (World (Environment time poss worldMap gen)) (Rabbit da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) =
    World (Environment time poss newMap gen)
    where
      satiatedAnimal = Left $ Rabbit $ eatDefaultAnimal da
      entities = worldMap !! x !! y
      newFood = die $ findFirstFood entities
      -- should never reach the empty list case, since we check beforehand that a food item exists
      findFirstFood (entity:entities) = if isFood $ isResource entity then entity else findFirstFood entities
      findFirstFood [] = Right (Grass 0.0 (x,y,0))
      isFood (Just grass@(Grass _ _)) = not $ isExpired grass
      isFood _ = False
      newMap = replaceEntityAt satiatedAnimal (replaceEntityAt newFood worldMap)

  -- drink is similar to eat, but for drinking. functionally, they are near identical (and ideally, if one were to refactor the code further, eat and drink (and maybe mate) would be abstracted out).
  drink (World (Environment time poss worldMap gen)) (Fox da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) =
    World (Environment time poss newMap gen)
    where
      quenchedAnimal = Left $ Fox $ drinkDefaultAnimal da
      entities = worldMap !! x !! y
      newDrink = die $ findFirstDrink entities
      -- should never reach the empty list case, since we check beforehand that a drink item exists
      findFirstDrink (entity:entities) = if isDrink $ isResource entity then entity else findFirstDrink entities
      findFirstDrink [] = Right (Water 0.0 (x,y,0))
      isDrink (Just water@(Water _ _)) = not $ isExpired water
      isDrink _ = False
      newMap = replaceEntityAt quenchedAnimal (replaceEntityAt newDrink worldMap)
  drink (World (Environment time poss worldMap gen)) (Rabbit da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) =
    World (Environment time poss newMap gen)
    where
      quenchedAnimal = Left $ Rabbit $ drinkDefaultAnimal da
      entities = worldMap !! x !! y
      newDrink = die $ findFirstDrink entities
      -- should never reach the empty list case, since we check beforehand that a drink item exists
      findFirstDrink (entity:entities) = if isDrink $ isResource entity then entity else findFirstDrink entities
      findFirstDrink [] = Right (Water 0.0 (x,y,0))
      isDrink (Just water@(Water _ _)) = not $ isExpired water
      isDrink _ = False
      newMap = replaceEntityAt quenchedAnimal (replaceEntityAt newDrink worldMap)

  -- mate is also similar to eat and drink, except it doesn't consume anything, but rather creates a new Animal using the createBabyAnimal helper and returns the updated world (with the animal). However, this new animal does not do anything until the next "turn"
  mate (World (Environment time poss worldMap gen)) (Fox da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) = 
    createBabyAnimal (World (Environment time poss satisfiedAnimalMap gen)) (Fox da) (Fox dan)
    where
      satisfiedAnimal1 = Left $ Fox $ mateDefaultAnimal da
      satisfiedAnimal2 = Left $ Fox $ mateDefaultAnimal dan
      newMap = worldMap
      entities = worldMap !! x !! y
      (Left firstValidMate) = head [mate | mate<-trace (show entities) entities, maybeCheckCanMate (Just (Fox da)) (isAnimal mate)]
      dan = getDefaultAnimal firstValidMate
      maybeCheckCanMate :: Maybe Animal -> Maybe Animal -> Bool
      maybeCheckCanMate (Just animal1) (Just animal2) = checkCanMate animal1 animal2
      maybeCheckCanMate _ _ = False
      satisfiedAnimalMap = replaceEntityAt satisfiedAnimal2 (replaceEntityAt satisfiedAnimal1 worldMap)
  mate (World (Environment time poss worldMap gen)) (Rabbit da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) = 
    createBabyAnimal (World (Environment time poss satisfiedAnimalMap gen)) (Rabbit da) (Rabbit dan)
    where
      satisfiedAnimal1 = Left $ Rabbit $ mateDefaultAnimal da
      satisfiedAnimal2 = Left $ Rabbit $ mateDefaultAnimal dan
      newMap = worldMap
      entities = worldMap !! x !! y
      (Left firstValidMate) = head [mate | mate<-entities, maybeCheckCanMate (Just (Rabbit da)) (isAnimal mate)]
      dan = getDefaultAnimal firstValidMate
      maybeCheckCanMate :: Maybe Animal -> Maybe Animal -> Bool
      maybeCheckCanMate (Just animal1) (Just animal2) = checkCanMate animal1 animal2
      maybeCheckCanMate _ _ = False
      satisfiedAnimalMap = replaceEntityAt satisfiedAnimal2 (replaceEntityAt satisfiedAnimal1 worldMap)

  -- traverses the grid around an animal's sense radius, and returns a subset of the map (just the entities that match the want (by calling the isOnWant on each slot)
  findWant (World (Environment _ _ worldMap _)) animal want =
    -- traverse the grid and pretend the animal is on it
    concat [filterWant animal (worldMap !! posibX !! posibY) want | (posibX, posibY)<-possiblePositions, isOnWant (updatePos animal (posibX,posibY,0)) want worldMap]
    where
      (ax,ay,az) = getPos animal
      senseR = getSenseRadius animal
      possiblePositions = [(x,y) | x<-[(ax-senseR)..(ax+senseR)], y<-[(ay-senseR)..(ay+senseR)], all (>=0) [x,y], x < length worldMap, y < length (worldMap !! x)]

  -- moves an animal to some new position on the map by removing it from the old position (and updating the z-values of all entities in the same slot (x,y)), and then adding it to the new position (at the end of the list in the slot (x,y))
  move world@(World (Environment time poss worldMap gen)) entity dst@(dstX,dstY,dstZ)
    -- if it is within its speed (can reach the tile on this turn
    | distanceToDst <= getSpeed entity = 
      let 
        -- update worldMap
        entitiesAtDst = removedEntityMap !! dstX !! dstY
        newMap = replaceEntityAt (updatePos (Left entity) (dstX,dstY,length entitiesAtDst)) removedEntityMap
        -- update poss (MAYBE NOT NEEDED, SINCE updateEntityList ALREADY DOES THIS) TODO?
        newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
      in World (Environment time newPoss newMap gen)
    -- if it is out of reach, then move the entity, but only speed units
        -- If distance along X axis is greater than speed, then walk along X axis speed units
        --  if the distance is shorter, then walk along X axis distance units
        --  then walk along Y axis speed-distance units
    | abs (dstX-srcX) > speed =
      if dstX > srcX then
        let 
          newDstX = clamp (0,length worldMap) (srcX+speed)
          entitiesAtDst = removedEntityMap !! newDstX !! srcY
          newMap = replaceEntityAt (updatePos (Left entity) (newDstX,srcY,length entitiesAtDst)) removedEntityMap
          newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
        in World (Environment time newPoss newMap gen)
      else
        let
          newDstX = clamp (0, length worldMap) (srcX-speed)
          entitiesAtDst = removedEntityMap !! newDstX !! srcY
          newMap = replaceEntityAt (updatePos (Left entity) (newDstX,srcY,length entitiesAtDst)) removedEntityMap
          newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
       in World (Environment time newPoss newMap gen)
    | otherwise =
      let
        xDistance = abs (dstX - srcX)
        yDistance = speed - xDistance
      in
        if dstX > srcX then
          if dstY > srcY then
            let 
              newDstX = clamp (0,length worldMap) (srcX+xDistance)
              newDstY = clamp (0,length (worldMap !! newDstX)) (srcY+yDistance)
              entitiesAtDst = removedEntityMap !! newDstX !! newDstY
              newMap = replaceEntityAt (updatePos (Left entity) (newDstX,newDstY,length entitiesAtDst)) removedEntityMap
              newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
            in World (Environment time newPoss newMap gen)
          else
            let 
              newDstX = clamp (0,length worldMap) (srcX+xDistance)
              newDstY = clamp (0,length (worldMap !! newDstX)) (srcY-yDistance)
              entitiesAtDst = removedEntityMap !! newDstX !! newDstY
              newMap = replaceEntityAt (updatePos (Left entity) (newDstX,newDstY,length entitiesAtDst)) removedEntityMap
              newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
            in World (Environment time newPoss newMap gen)
        else
          if dstY > srcY then
            let 
              newDstX = clamp (0,length worldMap) (srcX-xDistance)
              newDstY = clamp (0,length (worldMap !! newDstX)) (srcY+yDistance)
              entitiesAtDst = removedEntityMap !! newDstX !! newDstY
              newMap = replaceEntityAt (updatePos (Left entity) (newDstX,newDstY,length entitiesAtDst)) removedEntityMap
              newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
            in World (Environment time newPoss newMap gen)
          else
            let 
              newDstX = clamp (0,length worldMap) (srcX-xDistance)
              newDstY = clamp (0,length (worldMap !! newDstX)) (srcY-yDistance)
              entitiesAtDst = removedEntityMap !! newDstX !! newDstY
              newMap = replaceEntityAt (updatePos (Left entity) (newDstX,newDstY,length entitiesAtDst)) removedEntityMap
              newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
            in World (Environment time newPoss newMap gen)
    where
      speed = getSpeed entity
      distanceToDst = distance (entity, (dst,0)) 
      -- we need to split at this entity's Z position, and decrement every entity behind it by 1
      (srcX,srcY,srcZ) = getPos entity
      entities = worldMap !! srcX !! srcY
      (first, tempSecond) = splitAt srcZ entities
      second = if null tempSecond then tempSecond else tail tempSecond
      newSecond = [let (oldX,oldY,oldZ) = getPos k in updatePos k (oldX,oldY,oldZ-1)| k<-second]
      newEntities = first ++ newSecond
      (frontMiddle, tempRearMiddle) = splitAt srcY (worldMap !! srcX)
      rearMiddle = if null tempRearMiddle then tempRearMiddle else tail tempRearMiddle
      newY = frontMiddle ++ newEntities : rearMiddle
      (frontOuter, tempRearOuter) = splitAt srcX worldMap
      rearOuter = if null tempRearOuter then tempRearOuter else tail tempRearOuter
      removedEntityMap = frontOuter ++ newY : rearOuter

    

-- we define all the functions so that Resource can be an instance of Entity
instance Entity Resource where
  -- Resources cannot perform these actions, so they just return the unchanged world
  -- In addition, resources do not have wants, so arbitrary implementations are added to fully derive Entity. These SHOULD NEVER be called, and they generally will not be so long as getAction does not call any "action" and just returns a "progressed" world.
  eat world _ = world
  drink world _ = world
  mate world _ = world
  findWant _ _ _ = []
  move world _ _ = world
  getHunger _ = 0.0
  getThirst _ = 0.0
  getUrge _ = 0.0
  getSenseRadius _ = 0
  getSpeed _ = 0
  isOnWant _ _ _ = False
  filterWant _ _ _ = []
  wander world _ = world
  ageUp res = res

  -- returns the position of the resource
  getPos (Grass _ pos) = pos
  getPos (Water _ pos) = pos

  -- getAction for grass actually regrows some
  getAction (World (Environment time (h:poss) worldMap gen)) (Grass amt pos) =
    let newMap = replaceEntityAt (Right (Grass (amt+1.0) pos)) worldMap
    in World (Environment time poss newMap gen)
  getAction (World (Environment time (h:poss) worldMap gen)) _ = World (Environment time poss worldMap gen)

  -- A resource is expired when its amount is less than or equal to 0
  isExpired (Grass amt _) = amt <= 0
  isExpired (Water amt _) = amt <= 0

  -- updates the position of a resource
  updatePos (Grass amt pos) newPos = Grass amt newPos
  updatePos (Water amt pos) newPos = Water amt newPos

  -- gets the PosOrd of a resource. Since resources can't (shouldn't?) move, their speed is 0
  getPosOrd (Grass _ pos) = (pos,0)
  getPosOrd (Water _ pos) = (pos,0)

  -- isResource is an alias for Just, since any Resource is a resource
  isResource = Just
  -- isAnimal is Nothing, since a resource is not an animal
  isAnimal _ = Nothing

  -- die for Grass and Water behave differently, we just decrement a certain amount of resource, and return it
  die (Grass amt pos) = Grass (amt-1.0) pos
  die (Water amt pos) = Water (amt-1.0) pos

-- instance to help us abstract away calling on either, since the map is a 3D array of Either Animal World
instance (Entity l, Entity r) => Entity (Either l r) where
  -- trivial action functions, we just call the function on the desired inner entity
  eat world (Left a) = eat world a
  eat world (Right a) = eat world a

  drink world (Left a) = drink world a
  drink world (Right a) = drink world a

  mate world (Left a) = mate world a
  mate world (Right  a) = mate world a

  move world (Left a) = move world a
  move world (Right a) = move world a

  getAction world (Left a) = getAction world a
  getAction world (Right a) = getAction world a

  wander world (Left a) = wander world a
  wander world (Right a) = wander world a

  findWant world (Left a) want = findWant world a want
  findWant world (Right a) want = findWant world a want

  isOnWant (Left a) = isOnWant a
  isOnWant (Right a) = isOnWant a

  filterWant (Left a) lst want = filterWant a lst want
  filterWant (Right a) lst want = filterWant a lst want

  -- abstract definitions for getters, since we don't care if it's left or right, either way we call the corresponding getter
  getHunger = either (getHunger) (getHunger)
  getThirst = either (getThirst) (getThirst)
  getUrge = either (getUrge) (getUrge)
  getSenseRadius = either (getSenseRadius) (getSenseRadius)
  getSpeed = either (getSpeed)  (getSpeed)
  getPos = either (getPos) (getPos)
  isExpired = either (isExpired) (isExpired)
  getPosOrd = either (getPosOrd) (getPosOrd)
  isResource = either (isResource) (isResource)
  isAnimal = either (isAnimal) (isAnimal)

  -- special functions, since they must return the same type, so we need to re-wrap them after calling the unwrapped function
  --    pretty sure this can be done via some Monad functions/behaviours, but we were running short on time and this was a quick way to do it
  ageUp (Left a) = Left $ ageUp a
  ageUp (Right a) = Right $ ageUp a

  updatePos (Left a) pos = Left $ updatePos a pos
  updatePos (Right a) pos = Right $ updatePos a pos

  die (Left a) = Left $ die a
  die (Right a) = Right $ die a
