module Model.Ui exposing (..)

import Set exposing (Set)
import Keyboard exposing (KeyCode)

import Model.Scene exposing (..)


type alias Ui =
  { windowSize : (Int, Int)
  , pressedKeys : Set KeyCode
  , screen : Screen }


type Screen = StartScreen | PlayScreen | GameoverScreen

initialUi : Ui
initialUi =
  { windowSize = (800,640)
  , pressedKeys = Set.empty
  , screen = StartScreen }


keyPressed : KeyCode -> Set KeyCode -> Bool
keyPressed keycode pressedKeys =
  Set.member keycode pressedKeys
