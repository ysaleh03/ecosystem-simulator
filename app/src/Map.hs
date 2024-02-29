module Map (
  Map (..),
  Pos (..),
  OrdPos(..),
  makeEmptyMap
) where

import System.Random

type Map e = [[[e]]]
type Pos = (Int, Int, Int)
type OrdPos = (Pos, Int)

makeEmptyMap :: Int -> [[[t]]]
makeEmptyMap n =
  [[[] | _<-[1..n]] | _<-[1..n]]

merge2DArrays :: [[t]] -> [[t]] -> [[t]]
merge2DArrays [] [] = []
merge2DArrays (x:xs) (y:ys) = x : merge2DArrays xs ys
merge2DArrays _ _ = error "Arrays must have the same dimensions"


