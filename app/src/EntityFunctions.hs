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
  randomGrid3D,
  --merge3DArrays,
  makeEmptyMap,
  replaceEntityAt
) where

import Entity
import Data.List.Extras.Argmax
import Data.List
import Data.Maybe
import System.Random

-- initialize random  TODO
seed = 2402241714

-- Insert an element into a sorted list based on a comparison function
insertToSorted :: a -> [a] -> (a -> a -> Bool) -> [a]
insertToSorted e [] _ = [e]
insertToSorted e (h:t) comp
  | comp e h = e:(h:t)
  | otherwise = h:(insertToSorted e t comp)

-- get the position and speed pair from an entity
getPosOrdEntity :: Entity a => a -> PosOrd
getPosOrdEntity entity = (getPos entity, getSpeed entity)

-- get the distance between an entity and some position (ignores the speed of the PosOrd pair) on the x-y plane
distance :: Entity e => (e, PosOrd) -> Int
distance (e, ((x1,y1,_), _)) = 
  let ((x2,y2,_), _) = getPosOrd e in (abs (x2 - x1)) + (abs (y2 - y1))

{- MAP HELPERS -}
-- TODO: TEST MAP HELPERS

-- this was useful for splitting list https://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
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
checkCanMate (Fox (DefaultAnimal _ _ _ _ _ sex1 pos1 _)) (Fox (DefaultAnimal _ _ _ _ _ sex2 pos2 _)) =
  sex1 /= sex2 && pos1 == pos2
checkCanMate (Rabbit (DefaultAnimal _ _ _ _ _ sex1 pos1 _)) (Rabbit (DefaultAnimal _ _ _ _ _ sex2 pos2 _)) =
  sex1 /= sex2 && pos1 == pos2
checkCanMate _ _ = False

-- helper for returning the DefaultAnimal of an Animal
getDefaultAnimal :: Animal -> DefaultAnimal
getDefaultAnimal (Fox da) = da
getDefaultAnimal (Rabbit da) = da

-- creates a new Animal with inherited traits at the position of the parents, given two parents
createBabyAnimal :: World (Environment (Either Animal Resource)) -> Animal -> Animal -> World (Environment (Either Animal Resource))
createBabyAnimal (World (Environment time poss worldMap)) (Fox (DefaultAnimal h1 t1 u1 r1 s1 x1 (p11,p12,p13) l1)) (Fox (DefaultAnimal h2 t2 u2 r2 s2 x2 (p21,p22,p23) l2))
  | u1 > u2 = 
    let babyAnimal = Fox (DefaultAnimal 10.0 10.0 15.0 r1 s2 x1 (p11,p12,length (worldMap !! p11 !! p12)) 10)
        newMap = replaceEntityAt (Left babyAnimal) worldMap
    in World (Environment time poss newMap)
    
  | otherwise = 
    let babyAnimal = Fox (DefaultAnimal 10.0 10.0 15.0 r2 s1 x2 (p11,p12,length (worldMap !! p11 !! p12)) 10)
        newMap = replaceEntityAt (Left babyAnimal) worldMap
    in World (Environment time poss newMap)
createBabyAnimal (World (Environment time poss worldMap)) (Rabbit (DefaultAnimal h1 t1 u1 r1 s1 x1 (p11,p12,p13) l1)) (Rabbit (DefaultAnimal h2 t2 u2 r2 s2 x2 (p21,p22,p23) l2))
  | u1 > u2 = 
    let babyAnimal = Rabbit (DefaultAnimal 10.0 10.0 15.0 r1 s2 x1 (p11,p12,length (worldMap !! p11 !! p12)) 10)
        newMap = replaceEntityAt (Left babyAnimal) worldMap
    in World (Environment time poss newMap)
    
  | otherwise = 
    let babyAnimal = Rabbit (DefaultAnimal 10.0 10.0 15.0 r2 s1 x2 (p11,p12,length (worldMap !! p11 !! p12)) 10)
        newMap = replaceEntityAt (Left babyAnimal) worldMap
    in World (Environment time poss newMap)
createBabyAnimal world _ _ = world

{- END ANIMAL HELPERS -}

instance Entity Animal where
  -- this is used since we do not remove them until the end of the day cycle
  die (Fox (DefaultAnimal hunger thirst urge senseR speed sex pos lifetime)) =
    Fox (DefaultAnimal hunger thirst urge senseR speed sex pos (-1))
  die (Rabbit (DefaultAnimal hunger thirst urge senseR speed sex pos lifetime)) =
    Rabbit (DefaultAnimal hunger thirst urge senseR speed sex pos (-1))

  wander world@(World (Environment _ _ worldMap)) animal =
    --move world animal (srcX+changeInX,srcY+trueChangeInY,0)
    move world animal (clampChangeInX, clampChangeInY, length (worldMap !! clampChangeInX !! clampChangeInY))
    where
      (srcX,srcY,_) = getPos animal
      speed = getSpeed animal
      (changeInX,ngen1) = uniformR (-speed,speed) (mkStdGen seed)
      changeInY = (speed - (abs changeInX)) * (fst $ uniformR (-1,1) ngen1)
      trueChangeInY = if changeInY == 0 then speed - (abs changeInX) else changeInY
      clampChangeInX = clamp (0,(length worldMap) - 1) (srcX+changeInX)
      clampChangeInY = clamp (0,(length (worldMap !! clampChangeInX)) - 1) (srcY+trueChangeInY)

      clamp (min,max) val = if val > max then max else if val < min then min else val


  ageUp (Fox da) = Fox $ ageUpDefaultAnimal da
  ageUp (Rabbit da) = Rabbit $ ageUpDefaultAnimal da

  -- lifetime runs out, or some meter reaches 0
  isExpired (Fox (DefaultAnimal hunger thirst urge _ _ _ _ lifetime)) =
    any (<=0.0) [hunger, thirst] || lifetime <= 0
  isExpired (Rabbit (DefaultAnimal hunger thirst urge _ _ _ _ lifetime)) =
    any (<=0.0) [hunger, thirst] || lifetime <= 0

  updatePos (Fox da) pos = Fox $ updatePosDefaultAnimal da pos
  updatePos (Rabbit da) pos = Rabbit $ updatePosDefaultAnimal da pos

  getSpeed (Fox (DefaultAnimal _ _ _ _ speed _ _ _)) = speed
  getSpeed (Rabbit (DefaultAnimal _ _ _ _ speed _ _ _)) = speed

  getPos (Fox (DefaultAnimal _ _ _ _ _ _ pos _)) = pos
  getPos (Rabbit (DefaultAnimal _ _ _ _ _ _ pos _)) = pos

  -- single definition should work for all animals
  getPosOrd = getPosOrdEntity

  getHunger (Fox da) = getHungerDefaultAnimal da
  getHunger (Rabbit da) = getHungerDefaultAnimal da

  getThirst (Fox da) = getThirstDefaultAnimal da
  getThirst (Rabbit da) = getThirstDefaultAnimal da

  getUrge (Fox da) = getUrgeDefaultAnimal da
  getUrge (Rabbit da) = getUrgeDefaultAnimal da

  getSenseRadius (Fox da) = getSenseRadiusDefaultAnimal da
  getSenseRadius (Rabbit da) = getSenseRadiusDefaultAnimal da

  isResource _ = Nothing
  isAnimal = Just

  -- single definition should work for all animals
  getAction (World (Environment time (h:poss) worldMap)) animal
    -- it cannot sense an item it needs
    | null items = wander world animal
    -- it senses an item, check if it's in the same square (distance only counts x and y, not z)
    | distance (animal, getPosOrd firstItem) == 0  = action world animal
    -- it senses the item, but is not on it. Go towards the item
    | otherwise = move world animal (getPos firstItem)
    where
      world = (World (Environment time poss worldMap))
      ((want, action), _) = argmin snd (zip [(Food,eat),(Drink,drink),(Mate,mate)] [getHunger animal, getThirst animal, getUrge animal])
      items = (findWant world animal want)
      -- sort the list of items
      ordItems = sortOn distance (zip items (replicate (length items) (getPosOrd animal)))
      -- move to closest item
      firstItem = fst $ head ordItems

  isOnWant (Fox (DefaultAnimal _ _ _ _ _ _ (x,y,_) _)) Food worldMap = 
    or [containsFood $ isAnimal e | e<-entities]
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsFood (Just (Rabbit _)) = True
      containsFood _ = False
  isOnWant (Fox (DefaultAnimal _ _ _ _ _ _ (x,y,_) _)) Drink worldMap =
    or [containsDrink $ isResource e | e<-entities]
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsDrink (Just (Water _ _)) = True
      containsDrink _ = False
  isOnWant (Fox (DefaultAnimal _ _ _ _ _ firstSex (x,y,_) _)) Mate worldMap = 
    or [containsMate $ isAnimal e | e<-entities] 
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsMate (Just (Fox ( DefaultAnimal _ _ _ _ _ secondSex _ _))) = secondSex /= firstSex
      containsMate _ = False

  isOnWant (Rabbit (DefaultAnimal _ _ _ _ _ _ (x,y,_) _)) Food worldMap = 
    or [containsFood $ isResource e | e<-entities]
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsFood (Just (Grass _ _)) = True
      containsFood _ = False
  isOnWant (Rabbit (DefaultAnimal _ _ _ _ _ _ (x,y,_) _)) Drink worldMap =
    or [containsDrink $ isResource e | e<-entities]
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsDrink (Just (Water _ _)) = True
      containsDrink _ = False
  isOnWant (Rabbit (DefaultAnimal _ _ _ _ _ firstSex (x,y,_) _)) Mate worldMap = 
    or [containsMate $ isAnimal e | e<-entities]
    where
      -- entities should never be null, since at least the animal itself is at that position
      entities = worldMap !! x !! y
      containsMate (Just (Rabbit ( DefaultAnimal _ _ _ _ _ secondSex _ _))) = secondSex /= firstSex
      containsMate _ = False

  filterWant (Fox _) entities Food = 
    [e | e<-entities, containsFood $ isAnimal e]
    where
      containsFood (Just (Rabbit _)) = True
      containsFood _ = False
  filterWant (Fox _) entities Drink = 
    [e | e<-entities,containsDrink $ isResource e]
    where
      containsDrink (Just (Water _ _)) = True
      containsDrink _ = False
  filterWant (Fox (DefaultAnimal _ _ _ _ _ firstSex (x,y,_) _)) entities Mate = 
    [e| e<-entities, containsMate $ isAnimal e]
    where
      containsMate (Just (Fox ( DefaultAnimal _ _ _ _ _ secondSex _ _))) = secondSex /= firstSex
      containsMate _ = False
  filterWant (Rabbit _) entities Food = 
    [e | e<-entities, containsFood $ isResource e]
    where
      containsFood (Just (Grass _ _)) = True
      containsFood _ = False
  filterWant (Rabbit _) entities Drink = 
    [e | e<-entities,containsDrink $ isResource e]
    where
      containsDrink (Just (Water _ _)) = True
      containsDrink _ = False
  filterWant (Rabbit (DefaultAnimal _ _ _ _ _ firstSex (x,y,_) _)) entities Mate = 
    [e| e<-entities, containsMate $ isAnimal e]
    where
      containsMate (Just (Rabbit ( DefaultAnimal _ _ _ _ _ secondSex _ _))) = secondSex /= not firstSex
      containsMate _ = False

  eat (World (Environment time poss worldMap)) (Fox da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) =
    World (Environment time poss newMap)
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
  eat (World (Environment time poss worldMap)) (Rabbit da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) =
    World (Environment time poss newMap)
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

  drink (World (Environment time poss worldMap)) (Fox da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) =
    World (Environment time poss newMap)
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
  drink (World (Environment time poss worldMap)) (Rabbit da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) =
    World (Environment time poss newMap)
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

  mate (World (Environment time poss worldMap)) (Fox da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) = 
    createBabyAnimal (World (Environment time poss satisfiedAnimalMap)) (Fox da) (Fox dan)
    where
      satisfiedAnimal1 = Left $ Fox $ mateDefaultAnimal da
      satisfiedAnimal2 = Left $ Fox $ mateDefaultAnimal dan
      newMap = worldMap
      entities = worldMap !! x !! y
      (Left firstValidMate) = head [mate | mate<-entities, maybeCheckCanMate (Just (Fox da)) (isAnimal mate)]
      dan = getDefaultAnimal firstValidMate
      maybeCheckCanMate :: Maybe Animal -> Maybe Animal -> Bool
      maybeCheckCanMate (Just animal1) (Just animal2) = checkCanMate animal1 animal2
      maybeCheckCanMate _ _ = False
      satisfiedAnimalMap = replaceEntityAt satisfiedAnimal2 (replaceEntityAt satisfiedAnimal1 worldMap)
  mate (World (Environment time poss worldMap)) (Rabbit da@(DefaultAnimal hunger thirst urge senseR speed sex (x,y,z) lifetime)) = 
    createBabyAnimal (World (Environment time poss satisfiedAnimalMap)) (Rabbit da) (Rabbit dan)
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

  findWant (World (Environment _ _ worldMap)) animal want =
    -- traverse the grid and pretend the animal is on it
    concat [filterWant animal (worldMap !! posibX !! posibY) want | (posibX, posibY)<-possiblePositions, isOnWant (updatePos animal (posibX,posibY,0)) want worldMap]
    where
      (ax,ay,az) = getPos animal
      senseR = getSenseRadius animal
      possiblePositions = [(x,y) | x<-[(ax-senseR)..(ax+senseR)], y<-[(ay-senseR)..(ay+senseR)], all (>=0) [x,y], x < length worldMap, y < length (worldMap !! x)]

  move world@(World (Environment time poss worldMap)) entity dst@(dstX,dstY,dstZ)
    -- if it is within its speed (can reach the tile on this turn
    | distanceToDst <= getSpeed entity = 
      let 
        -- update worldMap
        entitiesAtDst = removedEntityMap !! dstX !! dstY
        newMap = replaceEntityAt (updatePos (Left entity) (dstX,dstY,length entitiesAtDst)) removedEntityMap
        -- update poss (MAYBE NOT NEEDED, SINCE updateEntityList ALREADY DOES THIS) TODO?
        newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
      in World (Environment time newPoss newMap)
    -- if it is out of reach, then move the entity, but only speed units
        -- If distance along X axis is greater than speed, then walk along X axis speed units
        --  if the distance is shorter, then walk along X axis distance units
        --  then walk along Y axis speed-distance units
    | abs (dstX-srcX) > speed =
      if dstX > srcX then
        let 
          newDstX = srcX+speed
          entitiesAtDst = removedEntityMap !! newDstX !! srcY
          newMap = replaceEntityAt (updatePos (Left entity) (newDstX,srcY,length entitiesAtDst)) removedEntityMap
          newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
        in World (Environment time newPoss newMap)
      else
        let
          newDstX = srcX-speed
          entitiesAtDst = removedEntityMap !! newDstX !! srcY
          newMap = replaceEntityAt (updatePos (Left entity) (newDstX,srcY,length entitiesAtDst)) removedEntityMap
          newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
       in World (Environment time newPoss newMap)
    | otherwise =
      let
        xDistance = abs (dstX - srcX)
        yDistance = speed - xDistance
      in
        if dstX > srcX then
          if dstY > srcY then
            let 
              newDstX = srcX+xDistance
              newDstY = srcY+yDistance
              entitiesAtDst = removedEntityMap !! newDstX !! newDstY
              newMap = replaceEntityAt (updatePos (Left entity) (newDstX,newDstY,length entitiesAtDst)) removedEntityMap
              newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
            in World (Environment time newPoss newMap)
          else
            let 
              newDstX = srcX+xDistance
              newDstY = srcY-xDistance
              entitiesAtDst = removedEntityMap !! newDstX !! newDstY
              newMap = replaceEntityAt (updatePos (Left entity) (newDstX,newDstY,length entitiesAtDst)) removedEntityMap
              newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
            in World (Environment time newPoss newMap)
        else
          if dstY > srcY then
            let 
              newDstX = srcX-xDistance
              newDstY = srcY+yDistance
              entitiesAtDst = removedEntityMap !! newDstX !! newDstY
              newMap = replaceEntityAt (updatePos (Left entity) (newDstX,newDstY,length entitiesAtDst)) removedEntityMap
              newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
            in World (Environment time newPoss newMap)
          else
            let 
              newDstX = srcX-xDistance
              newDstY = srcY-xDistance
              entitiesAtDst = removedEntityMap !! newDstX !! newDstY
              newMap = replaceEntityAt (updatePos (Left entity) (newDstX,newDstY,length entitiesAtDst)) removedEntityMap
              newPoss = [if (px==srcX && py==srcY && pz>srcZ) then ((px,py,pz-1),spd) else ppos | ppos@((px,py,pz),spd)<-poss]
            in World (Environment time newPoss newMap)
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

    

instance Entity Resource where
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

  getPos (Grass _ pos) = pos
  getPos (Water _ pos) = pos

  getAction (World (Environment time (h:poss) worldMap)) _ = (World (Environment time poss worldMap))

  isOnWant _ _ _ = False
  filterWant _ _ _ = []
  wander world _ = world
  ageUp res = res

  isExpired (Grass amt _) = amt <= 0
  isExpired (Water amt _) = amt <= 0

  updatePos (Grass amt pos) newPos = Grass amt newPos
  updatePos (Water amt pos) newPos = Grass amt newPos

  getPosOrd (Grass _ pos) = (pos,0)
  getPosOrd (Water _ pos) = (pos,0)

  isResource = Just
  isAnimal _ = Nothing

  -- die for Grass and Water behave differently, we just decrement a certain amount of resource
  die (Grass amt pos) = Grass (amt-10.0) pos
  die (Water amt pos) = Water (amt-10.0) pos

instance (Entity l, Entity r) => Entity (Either l r) where

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

  findWant world (Left a) want = [t | t<-findWant world a want]
  findWant world (Right a) want = [t | t<-findWant world a want]

  getHunger = either (getHunger) (getHunger)
  getThirst = either (getThirst) (getThirst)
  getUrge = either (getUrge) (getUrge)
  getSenseRadius = either (getSenseRadius) (getSenseRadius)
  getSpeed = either (getSpeed)  (getSpeed)
  getPos = either (getPos) (getPos)

  isOnWant (Left a) = isOnWant a
  isOnWant (Right a) = isOnWant a

  filterWant (Left a) lst want = [t | t<-filterWant a lst want]
  filterWant (Right a) lst want = [t | t<-filterWant a lst want]

  ageUp (Left a) = Left $ ageUp a
  ageUp (Right a) = Right $ ageUp a

  isExpired = either (isExpired) (isExpired)

  updatePos (Left a) pos = Left $ updatePos a pos
  updatePos (Right a) pos = Right $ updatePos a pos

  getPosOrd = either (getPosOrd) (getPosOrd)

  isResource = either (isResource) (isResource)
  isAnimal = either (isAnimal) (isAnimal)

  die (Left a) = Left $ die a
  die (Right a) = Right $ die a


{-
  getAction world@(World (Environment time poss map)) a@(Fox (DefaultAnimal hunger thirst urge senseR speed sex pos lifetime)) = 
      if isOnWant a want map then
        action world a
      else 
        if null items then 
          wander world a
        else
          move world a (fst $ getPosOrd firstItem)
        where
          ((want, action), num) = argmin snd (zip [(Food,eat),(Drink,drink),(Mate,mate)] [getHunger a, getThirst a, getUrge a])
          items = findWant world a want
          -- sort the list of items
          ordItems = sortOn distance (zip items (replicate (length items) (pos, speed)))
          -- move to closest item
          firstItem = fst $ head ordItems
-}

