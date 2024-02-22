module Main where

import Control.Monad
import Graphics.UI.Gtk

main :: IO ()
main = do
  void initGUI
  
  window <- windowNew
  void $ on window objectDestroy mainQuit

  set window [windowDefaultWidth := 400,
              windowDefaultHeight := 400,
              windowResizable := False,
              windowTitle := "Simulator"]
  
  grid <- gridNew
  containerAdd window grid

  mapLabel <- labelNew (Just "Map goes here")
  gridAttach grid mapLabel 0 0 1 1

  sidebar <- labelNew (Just "Status goes here")
  gridAttach grid sidebar 1 0 1 1

  startButton <- buttonNewWithLabel "Start"
  pauseButton <- buttonNewWithLabel "Pause"
  stopButton  <- buttonNewWithLabel "Stop"

  gridAttach grid startButton 0 1 1 1
  gridAttach grid pauseButton 1 1 1 1
  gridAttach grid stopButton  2 1 1 1

  widgetShowAll window
  mainGUI
