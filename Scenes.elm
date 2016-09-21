module Scenes exposing (..)

import Dict
import Array exposing (..)


type alias Scene =
    { tiles : Array Int
    , tileWidth : Int
    , tileHeight : Int
    , sceneWidth : Int
    , sceneHeight :
        Int
        -- player spawn
    , spawnX : Int
    , spawnY : Int
    }


type alias Tile =
    { tileType : Int
    , x : Float
    , y : Float
    , w : Float
    , h : Float
    }


tileAt : Scene -> Int -> Int -> Tile
tileAt scene index _ =
    let
        i =
            (index % scene.sceneWidth)

        j =
            (index // scene.sceneWidth)

        x =
            i * scene.tileWidth

        y =
            j * scene.tileHeight
    in
        { tileType = Maybe.withDefault 0 (Array.get index scene.tiles)
        , x = toFloat x
        , y = toFloat y
        , w = toFloat scene.tileWidth
        , h = toFloat scene.tileHeight
        }


tiles : Scene -> Array Tile
tiles scene =
    Array.indexedMap (tileAt scene) scene.tiles


windowSize : Scene -> ( Int, Int )
windowSize scene =
    ( scene.tileWidth * (scene.sceneWidth), scene.tileHeight * (scene.sceneHeight) )


scenes : Dict.Dict String Scene
scenes =
    Dict.fromList
        [ ( "test", genTestScene ) ]


getScene : String -> Maybe Scene
getScene sceneName =
    Dict.get sceneName scenes


genTestScene : Scene
genTestScene =
    let
        sceneWidth =
            25

        sceneHeight =
            20

        genTestTiles index =
            let
                x =
                    index % sceneWidth

                -- dX =
                --     Debug.log ("x: " ++ toString x) 1
                y =
                    index // sceneWidth

                -- dY =
                --     Debug.log ("y: " ++ toString y) 1
            in
                if x == 0 || x == (sceneWidth - 1) || y == 0 || y >= (sceneHeight - 3) then
                    1
                else if x == 12 && y == 10 then
                    2
                else
                    0
    in
        { tiles = Array.initialize (sceneWidth * sceneHeight) genTestTiles
        , tileWidth = 32
        , tileHeight = 32
        , sceneWidth = sceneWidth
        , sceneHeight = sceneHeight
        , spawnX = sceneWidth // 2
        , spawnY = sceneHeight // 2
        }


getPlayerSpawnPos : Scene -> { x : Float, y : Float }
getPlayerSpawnPos scene =
    { x = toFloat (scene.spawnX * scene.tileWidth), y = toFloat (scene.spawnY * scene.tileHeight) }
