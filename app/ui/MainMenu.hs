module MainMenu where

import Control.Monad
import Data.Maybe
import Text.Read
import System.IO
import Simulator 

newMenuWindow :: IO ()
newMenuWindow = do
  hSetBuffering stdout NoBuffering
  putStr "*** Welcome to Ecosystem Simulator ***\n"
  putStr "To start a new Simulation, please enter the following:\n"
  putStr "Will default to 10 100 100"
  
  putStr "\n1. Map dimension: "
  dim <- getLine
  putStr "\n2. Number of entities to add: "
  num <- getLine
  
  putStr "*** Ecosystem Simulator ***"
  putStr ("Map size = " ++ dim ++ ", Init entities = " ++ num)

  getSimulator (fromMaybe 10 (readMaybe dim)) (fromMaybe 100 (readMaybe num))
