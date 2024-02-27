module MainMenu where

import Control.Monad
--import Graphics.UI.Gtk
import Simulator

newMenuWindow :: IO ()
newMenuWindow = getSimulator
{-
  void initGUI

  window <- windowNew
  void $ on window objectDestroy mainQuit

  set window [windowTitle := "Main Menu",
              windowDefaultWidth := 200,
              windowDefaultHeight := 150,
              containerBorderWidth := 10]

  vbox <- vBoxNew False 5
  containerAdd window vbox

  simButton  <- buttonNewWithLabel "Simulator"
  credButton <- buttonNewWithLabel "Credits"
  wikiButton <- buttonNewWithLabel "Wiki Page"

  _ <- on simButton buttonActivated $ do
    simulator <- newSimulatorWindow
    widgetShowAll simulator

  boxPackStart vbox simButton  PackNatural 0
  boxPackStart vbox credButton PackNatural 1
  boxPackStart vbox wikiButton PackNatural 2
 
  widgetShowAll window
  return window
-}
