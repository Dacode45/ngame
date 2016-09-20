module Main exposing (..)

import Html exposing (Html)
import Html.App as Html

import Svg.Events exposing (onClick)
import Svg.Attributes as Attributes exposing (x,y,width,height,fill,fontFamily,textAnchor)

main : Program Never
main =
  Html.program
  {
    init = init
    update = update
    view = view
    subscriptions = subscriptions
  }

type alias Game :
  {
      scene : Scene
  }

type alias Scene :
  {
    tiles : Array Int
    ,tileWidth : Float
    ,tileHeight : Float
    ,sceneWidght : Int
    ,sceneHeight : Int
  }

type alias Tile :
  {
    tileType : Int
    x : Float
    y : Float
    w : Float
    h : Float
  }

tileAt : Scene -> Int -> Int -> Tile
tileAt scene index _
  let
    i = index % sceneWidth
    j = index // sceneWidth
    x = i * scene.tileWidth
    y = j * scene.tileHeight
  in
  {
    tileType = Array.get index scene.tiles
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
  (scene.tileWidth * scene.sceneWidth, scene.tileHeight * scene.sceneHeight)

-- Subscriptions

type Msg
 = ResizeWindow (Int, Int)
  | Tick Time
  | KeyChange Bool KeyCode
  | StartGame
  | TimeSecond Time
  | NoOp

subscriptions : Game -> Sub Msg
subscriptions game =
  let
    window = Window.resizes (\{width,height} -> ResizeWindow (width,height))
    keys = [ Keyboard.downs (KeyChange True)
           , Keyboard.ups (KeyChange False)
             ]
    animation = [ AnimationFrame.diffs Tick ]
  in
    ( window ++ keys ++ animation ) |> Sub.batch

initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
  Task.perform (\_ -> NoOp) (\{width,height} -> ResizeWindow (width,height)) Window.size
-- View --
view : Game -> Html Msg
view game =
  renderScene game.scene

renderScene : Scene -> Html.Html Msg
renderscene scene =
  Svg.svg(svgAttributes ( windowSize scene))
  [
    renderMap scene
  ]

tileColor : Int -> String
tileColor tileType =
  case tileType of
    1 -> "rgba(255, 0, 0, 1)"
    _ -> "rgba(255, 255, 255, 0)"

renderTile : Tile -> Svg Msg
renderTile tile
let
  xString = toString tile.x
  yString = toString tile.y
  widthString = toString tile.w
  heightString = toString tile.h
in
  Svg.rect
  [
    x xString
    ,y yString
    , width  widthString
    , height heightString
    , fill tileColor tile
  ]

renderMap : Scene -> Svg Msg
renderMap scene =
  scene
    |> tiles
    |> Array.map renderTile
    |> toList

svgAttributes : (Int, Int) -> List (Attribute Msg)
svgAttributes (w, h) =
  [ width (toString w)
  , height (toString h)
  , Attributes.viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)
  , VirtualDom.property "xmlns:xlink" (Json.string "http://www.w3.org/1999/xlink")
  , Attributes.version "1.1"
  , Attributes.style "position: fixed;"
  ]
