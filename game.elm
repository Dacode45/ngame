module Main exposing (..)

import Html exposing (Html)
import Html.App as Html
import Array exposing (Array)
import Time exposing (Time)
import Window
import Scenes exposing (..)
import Physics exposing (..)
import AnimationFrame
import Keyboard exposing (KeyCode)
import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes exposing (x, y, width, height, fill, fontFamily, textAnchor)
import VirtualDom
import Json.Encode as Json
import Task


main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Game =
    { scene : Scene
    , player : GameObject
    }


type alias GameObject =
    { position : Vector
    }



-- Initialization


init : ( Game, Cmd Msg )
init =
    let
        initialScene =
            Maybe.withDefault genTestScene (getScene "test")
    in
        ( { scene = initialScene
          , player = GameObject (getPlayerSpawnPos initialScene)
          }
        , Cmd.none
        )



-- Update


update : Msg -> Game -> ( Game, Cmd Msg )
update action game =
    ( game, Cmd.none )



-- Subscriptions


type Msg
    = ResizeWindow ( Int, Int )
    | Tick Time
    | KeyChange Bool KeyCode
    | StartGame
    | TimeSecond Time
    | NoOp


subscriptions : Game -> Sub Msg
subscriptions game =
    let
        window =
            Window.resizes (\{ width, height } -> ResizeWindow ( width, height ))

        keys =
            [ Keyboard.downs (KeyChange True)
            , Keyboard.ups (KeyChange False)
            ]

        animation =
            [ AnimationFrame.diffs Tick ]
    in
        ([ window ] ++ keys ++ animation) |> Sub.batch


initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
    Task.perform (\_ -> NoOp) (\{ width, height } -> ResizeWindow ( width, height )) Window.size



-- View --


view : Game -> Html Msg
view game =
    Svg.svg (svgAttributes (windowSize game.scene))
        ((renderScene game.scene) ++ [ renderPlayer game.scene game.player ])


renderPlayer : Scene -> GameObject -> Svg Msg
renderPlayer scene player =
    let
        xString =
            toString player.position.x

        yString =
            toString player.position.y

        widthString =
            toString (scene.tileWidth // 2)

        heightString =
            toString scene.tileHeight

        msg =
            Debug.log (toString player) 1
    in
        Svg.rect
            [ x xString
            , y yString
            , width widthString
            , height heightString
            , fill "rgba(0,0,255,1)"
            ]
            []


renderScene : Scene -> List (Svg Msg)
renderScene scene =
    renderMap scene


tileColor : Int -> String
tileColor tileType =
    case tileType of
        1 ->
            "rgba(255, 0, 0, 1)"

        _ ->
            "rgba(255, 255, 255, 0)"


renderTile : Tile -> Svg Msg
renderTile tile =
    let
        xString =
            toString tile.x

        yString =
            toString tile.y

        widthString =
            toString tile.w

        heightString =
            toString tile.h
    in
        Svg.rect
            [ x xString
            , y yString
            , width widthString
            , height heightString
            , fill (tileColor tile.tileType)
            ]
            []


renderMap : Scene -> List (Svg Msg)
renderMap scene =
    let
        t =
            1

        --Debug.log (toString scene.tiles) 1
    in
        scene
            |> tiles
            |> Array.map renderTile
            |> Array.toList


svgAttributes : ( Int, Int ) -> List (Attribute Msg)
svgAttributes ( w, h ) =
    [ width (toString w)
    , height (toString h)
    , Attributes.viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)
    , VirtualDom.property "xmlns:xlink" (Json.string "http://www.w3.org/1999/xlink")
    , Attributes.version "1.1"
    , Attributes.style "position: fixed;"
    ]
