module Map (
  Map (..),
  Pos (..),
  OrdPos(..),
  randomGrid3D,
  merge3DArrays,
  makeEmptyMap
) where

import System.Random

type Map e = [[[e]]]
type Pos = (Int, Int, Int)
type OrdPos = (Pos, Int)

makeEmptyMap :: Int -> [[[t]]]
makeEmptyMap n =
  [[[] | _<-[1..n]] | _<-[1..n]]

merge3DArrays :: [[[t]]] -> [[[t]]] -> [[[t]]]
merge3DArrays [] [] = []
merge3DArrays (x:xs) (y:ys) = merge2DArrays x y : merge3DArrays xs ys
merge3DArrays _ _ = error "Arrays must have the same dimensions"

merge2DArrays :: [[t]] -> [[t]] -> [[t]]
merge2DArrays [] [] = []
merge2DArrays (x:xs) (y:ys) = x : merge2DArrays xs ys
merge2DArrays _ _ = error "Arrays must have the same dimensions"

--data MapDefaultAnimal = MapDefaultAnimal Double Double Double Int Int Bool (Int, Int, Int)

--data MapAnimal = Fox MapDefaultAnimal | Rabbit MapDefaultAnimal

-- instance Show Animal where
  -- show (Fox _) = "Fox"
  -- show (Rabbit _) = "Rabbit"

-- Function to generate a random 3D grid of size n x n x n
randomGrid3D :: Int -> t -> Map t
randomGrid3D n animalType = 
  replicate n (randomGrid2D n animalType)

-- Function to generate a random 2D grid of size n x n
randomGrid2D :: Int -> t -> [[t]]
-- TODO: REPLACE THIS SET NUMBER WITH MAYBE OPTION FOR GEN???
randomGrid2D n animalType = 
  let fstGen = mkStdGen 22020241746
  in fst $ unzip $ foldr (\a ( (animals,gen) :t) -> (randomBit a gen) : ((animals,gen):t)) [randomBit animalType fstGen] [animalType | _<-replicate (n-1) 0]

-- Function to generate a random bit
randomBit :: t -> StdGen -> ([t], StdGen)
randomBit animalType gen = 
  let rand = randomR (0,1) gen in
  if fst rand == (1 :: Int) then
    ([animalType], snd rand)
  else
    ([], snd rand)

-- Function to print the 3D grid
printGrid3D :: Show t => Map t -> IO ()
printGrid3D = mapM_ printGrid2D

-- Function to print a 2D grid
printGrid2D :: Show t => [[t]] -> IO ()
printGrid2D = mapM_ (putStrLn . concatMap show)


