module Scenes exposing (..)
import Dict
import Array exposing (..)

type alias Scene =
  {
    tiles : Array Int
    ,tileWidth : Float
    ,tileHeight : Float
    ,sceneWidth : Int
    ,sceneHeight : Int
  }

type alias Tile =
  {
    tileType : Int
    ,x : Float
    ,y : Float
    ,w : Float
    ,h : Float
  }

tileAt : Scene -> Int -> Int -> Tile
tileAt scene index _ =
  let
    i = toFloat (index % scene.sceneWidth)
    j = toFloat (index // scene.sceneWidth)
    x = i * scene.tileWidth
    y = j * scene.tileHeight
  in
  {
    tileType = Maybe.withDefault 0  (Array.get index scene.tiles)
    , x = x
    , y = y
    , w = scene.tileWidth
    , h = scene.tileHeight
  }

tiles : Scene -> Array Tile
tiles scene =
  Array.indexedMap (tileAt scene) scene.tiles

windowSize : Scene -> (Float, Float)
windowSize scene =
  (scene.tileWidth * (toFloat scene.sceneWidth), scene.tileHeight * (toFloat scene.sceneHeight))

scenes : Dict.Dict String Scene
scenes = Dict.fromList
  [ ("test", genTestScene)]

getScene : String -> Maybe Scene
getScene sceneName =
  Dict.get sceneName scenes

genTestScene : Scene
genTestScene =
  let
    sceneWidth = 25
    sceneHeight = 20
    genTestTiles = (\index -> if index == 0 || index % sceneWidth == 0 || index % sceneWidth == sceneWidth-1 || index > sceneWidth*(sceneHeight-3) then 1 else 0)
  in
    {
      tiles = Array.initialize (sceneWidth*sceneHeight) genTestTiles
      , tileWidth = 32
      , tileHeight = 32
      , sceneWidth = 32
      , sceneHeight = 32
    }
