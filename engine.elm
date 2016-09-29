module Main exposing (..)

import Svg exposing (Svg, Attribute)
import Svg.Attributes as Attributes
import Html exposing (Html)
import Html.App as Html
import VirtualDom
import Json.Encode as Json
import Time exposing (Time)
import AnimationFrame
import Dict
import Array exposing (Array)
import Set exposing (Set)
import Keyboard exposing (KeyCode)


main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Color =
    String


type alias Input =
    { x : Int, y : Int, action : Bool }


type alias Entity =
    { components : Dict.Dict String Component
    , tag : String
    , name : String
    , layer : Int
    }


redBall : Entity
redBall =
    { tag = "player"
    , name = "redBall"
    , layer = 0
    , components =
        Dict.fromList
            [ ( "position", (Position 10 10) )
            , ( "velocity", Velocity 0 0 )
            , ( "color", Color "rgba(255, 0, 0, 1)" )
            , ( "shape", Shape (Square 1) )
            , ( "controllable", Controllable (Input 0 0 False) )
            , ( "collision", Collision [] )
            , ( "collisionEnter", CollisionEnter ninjaPlayer )
            , ( "boundingBox", BoundingBox 1 1 )
            , ( "physics", Physics (Kinematic 10) (Elastic 0.1) )
            ]
    }


blueSquare : Entity
blueSquare =
    { tag = "wall"
    , name = "blueSquare"
    , layer = 0
    , components =
        Dict.fromList
            [ ( "position", (Position 4 2) )
            , ( "velocity", Velocity 0 0 )
            , ( "color", Color "rgba(0, 0, 255, 1)" )
            , ( "collision", Collision [] )
            , ( "player", Player )
            , ( "shape", Shape (Square 1) )
            , ( "boundingBox", BoundingBox 1 1 )
            , ( "physics", Physics (Kinematic 10) (Elastic 0.1) )
            ]
    }


type ShapeType
    = Square Float
    | Circle Float


type CollisionType
    = Elastic Float
    | Displace


type PhysicsType
    = Dynamic
      -- Motion and Gravity
    | Kinematic Float
      -- Motion Gravity
    | Static



-- No Motion


type Component
    = Position Float Float
    | Velocity Float Float
    | Color Color
    | Shape ShapeType
    | BoundingBox Float Float
    | Controllable Input
    | Player
    | Physics PhysicsType CollisionType
    | Collision (List Entity)
    | CollisionEnter (Time.Time -> Entity -> Entity)


type Msg
    = Tick Time
    | KeyChange Bool KeyCode


type alias TileMap =
    { tiles : Array Int
    , tile : Int -> Maybe Entity
    , mapLength : Int
    , mapHeight : Int
    , layer : Int
    }


getTile : TileMap -> Int -> Int -> Maybe Entity
getTile tilemap index _ =
    let
        x =
            index % tilemap.mapLength

        y =
            index // tilemap.mapHeight
    in
        tileAt tilemap x y


tileAt : TileMap -> Int -> Int -> Maybe Entity
tileAt tileMap x y =
    let
        index =
            y * tileMap.mapLength + x
    in
        case Array.get index tileMap.tiles of
            Just tileType ->
                case (tileMap.tile tileType) of
                    Just entity ->
                        Just (upsertComponent "position" (Position (toFloat x) (toFloat y)) entity)

                    Nothing ->
                        Nothing

            _ ->
                Nothing


getTiles : TileMap -> List Entity
getTiles tileMap =
    tileMap.tiles
        |> Array.indexedMap (getTile tileMap)
        |> Array.toList
        |> filterJusts


type alias Scene =
    { sceneWidth : Int
    , sceneHeight : Int
    , tileWidth : Float
    , tileMaps : Array TileMap
    }


loadEntities : Scene -> List Entity
loadEntities scene =
    scene.tileMaps
        |> Array.toList
        |> List.concatMap getTiles


type alias Game =
    { entities : List Entity
    , input : Input
    , scene : Scene
    }


init : ( Game, Cmd Msg )
init =
    let
        scene =
            buildTestScene
    in
        ( { entities = [ redBall, blueSquare ] ++ (loadEntities scene)
          , input = Input 0 0 False
          , scene = scene
          }
        , Cmd.none
        )


subscriptions : Game -> Sub Msg
subscriptions game =
    let
        keys =
            [ Keyboard.downs (KeyChange True)
            , Keyboard.ups (KeyChange False)
            ]

        animation =
            [ AnimationFrame.diffs Tick ]
    in
        (keys ++ animation) |> Sub.batch


update : Msg -> Game -> ( Game, Cmd Msg )
update msg ({ input } as game) =
    case msg of
        Tick dt ->
            let
                debug =
                    Debug.log "update" 1
            in
                ( { game
                    | entities = List.map (updateEntity (Time.inSeconds dt) game) game.entities
                  }
                , Cmd.none
                )

        KeyChange pressed code ->
            ( handleKeyChange pressed code game
                |> applyInputGame
            , Cmd.none
            )


updateEntity : Time.Time -> Game -> Entity -> Entity
updateEntity dt game entity =
    entity
        |> applyGravity dt
        |> moveEntity dt
        |> checkCollisions game.entities
        |> resolveCollisions
        |> onCollisionEnter dt


handleKeyChange : Bool -> KeyCode -> Game -> Game
handleKeyChange pressed keycode game =
    let
        input =
            game.input
    in
        if pressed then
            if keycode == 87 then
                { game
                    | input = { input | y = -1 }
                }
            else if keycode == 83 then
                { game
                    | input = { input | y = 1 }
                }
            else if keycode == 65 then
                { game
                    | input = { input | x = -1 }
                }
            else if keycode == 68 then
                { game
                    | input = { input | x = 1 }
                }
            else if keycode == 32 then
                { game
                    | input = { input | action = pressed }
                }
            else
                game
        else if keycode == 87 then
            { game
                | input = { input | y = 0 }
            }
        else if keycode == 83 then
            { game
                | input = { input | y = 0 }
            }
        else if keycode == 65 then
            { game
                | input = { input | x = 0 }
            }
        else if keycode == 68 then
            { game
                | input = { input | x = 0 }
            }
        else if keycode == 32 then
            { game
                | input = { input | action = pressed }
            }
        else
            game


view : Game -> Html Msg
view ({ scene, entities } as game) =
    let
        debug =
            Debug.log "render" 1
    in
        render ( scene.sceneWidth, scene.sceneHeight, scene.tileWidth ) entities



-- Action: Render entity


renderEntity : Float -> Entity -> Maybe (Svg Msg)
renderEntity scale entity =
    case
        ( getComponent "position" entity
        , getComponent "shape" entity
        , getComponent "color" entity
        )
    of
        ( Just (Position x y), Just (Shape shape), Just (Color color) ) ->
            case shape of
                Square width ->
                    Just
                        (Svg.rect
                            [ Attributes.x (toString (x * scale))
                            , Attributes.y (toString (y * scale))
                            , Attributes.width (toString (1 + (width * scale)))
                            , Attributes.height (toString (1 + (width * scale)))
                            , Attributes.fill (color)
                            ]
                            []
                        )

                Circle radius ->
                    Just
                        (Svg.circle
                            [ Attributes.cx (toString ((x + radius) * scale))
                            , Attributes.cy (toString ((y + radius) * scale))
                            , Attributes.r (toString (1 + (radius * scale)))
                            , Attributes.fill (color)
                            ]
                            []
                        )

        _ ->
            Nothing


render : ( Int, Int, Float ) -> List Entity -> Html Msg
render ( screenWidth, screenHeight, tileWidth ) entities =
    Svg.svg (svgAttributes ( screenWidth, screenHeight ))
        (entities
            |> List.sortBy .layer
            |> List.map (renderEntity tileWidth)
            |> filterJusts
        )


svgAttributes : ( Int, Int ) -> List (Attribute Msg)
svgAttributes ( w, h ) =
    [ Attributes.width (toString w)
    , Attributes.height (toString h)
    , Attributes.viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)
    , VirtualDom.property "xmlns:xlink" (Json.string "http://www.w3.org/1999/xlink")
    , Attributes.version "1.1"
    , Attributes.style "position: fixed;"
    ]



-- Action: Apply gravity on an object
-- Must have a velocity component and Kinematic physics


type alias CollisionDirection =
    Int


cTop : CollisionDirection
cTop =
    0


cBottom : CollisionDirection
cBottom =
    1


cLeft : CollisionDirection
cLeft =
    2


cRight : CollisionDirection
cRight =
    3


toCollisionDir : Entity -> Entity -> Maybe CollisionDirection
toCollisionDir a b =
    case ( getComponent "position" a, getComponent "boundingBox" a ) of
        ( Just (Position ax ay), Just (BoundingBox aw ah) ) ->
            case ( getComponent "position" b, getComponent "boundingBox" b ) of
                ( Just (Position bx by), Just (BoundingBox bw bh) ) ->
                    let
                        aMidX =
                            ax + (aw / 2)

                        aMidY =
                            ay + (ah / 2)

                        bMidX =
                            bx + (bw / 2)

                        bMidY =
                            by + (bh / 2)

                        dx =
                            (bMidX - aMidX)

                        dy =
                            (bMidY - aMidY)

                        absDx =
                            abs dx

                        absDy =
                            abs dy
                    in
                        if absDx > absDy then
                            if bx > ax then
                                Just cRight
                            else
                                Just cLeft
                        else if by > ay then
                            Just cBottom
                        else
                            Just cTop

                _ ->
                    Nothing

        _ ->
            Nothing


collisionDir : Entity -> List Entity -> Set CollisionDirection
collisionDir entity entities =
    entities
        |> List.map (toCollisionDir entity)
        |> filterJusts
        |> Set.fromList


ninjaPlayer : Time.Time -> Entity -> Entity
ninjaPlayer dt entity =
    case
        ( getComponent "controllable" entity
        , getComponent "collision" entity
        , getComponent "velocity" entity
        )
    of
        ( Just (Controllable input), Just (Collision entities), Just (Velocity vx vy) ) ->
            let
                collisionSet =
                    collisionDir entity entities

                ix =
                    toFloat input.x

                iy =
                    toFloat input.y

                jump =
                    input.action

                jumpSpeed =
                    10

                acc =
                    10

                maxSpeed =
                    50

                maxSlideSpeed =
                    20

                friction =
                    2

                handleWalking : Entity -> Entity
                handleWalking entity =
                    if (vx > maxSpeed) || (ix == 0 && iy == 0) then
                        upsertComponent "velocity" (Velocity (vx - (friction * vx * dt)) vy) entity
                    else
                        upsertComponent "velocity" (Velocity (vx + (dt * acc * ix)) vy) entity

                handleJumping : Entity -> Entity
                handleJumping entity =
                    if Set.member cBottom collisionSet then
                        if jump then
                            upsertComponent "velocity" (Velocity vx (-jumpSpeed)) entity
                        else
                            entity
                    else
                        entity

                handleWallJumping : Entity -> Entity
                handleWallJumping entity =
                    if jump then
                        if Set.member cLeft collisionSet then
                            upsertComponent "velocity" (Velocity (jumpSpeed / 2) (-jumpSpeed)) entity
                        else if Set.member cRight collisionSet then
                            upsertComponent "velocity" (Velocity (-jumpSpeed / 2) (-jumpSpeed)) entity
                        else
                            entity
                    else
                        entity

                handleWallSliding : Entity -> Entity
                handleWallSliding entity =
                    if (Set.member cLeft collisionSet || Set.member cRight collisionSet) then
                        upsertComponent "velocity" (Velocity vx 1) entity
                    else
                        entity
            in
                entity
                    |> handleWalking
                    |> handleWallSliding
                    |> handleWallJumping
                    |> handleJumping

        _ ->
            entity


applyGravity : Time.Time -> Entity -> Entity
applyGravity dt entity =
    case ( getComponent "velocity" entity, getComponent "physics" entity ) of
        ( Just (Velocity x y), Just (Physics (Kinematic gravity) _) ) ->
            upsertComponent "velocity" (Velocity x (y + (dt * gravity))) entity

        _ ->
            entity


moveEntity : Time.Time -> Entity -> Entity
moveEntity dt entity =
    case ( getComponent "position" entity, getComponent "velocity" entity ) of
        ( Just (Position x y), Just (Velocity vx vy) ) ->
            upsertComponent "position" (Position (x + (dt * vx)) (y + (dt * vy))) entity

        _ ->
            entity


applyInput : Input -> Entity -> Entity
applyInput input entity =
    case (getComponent "controllable" entity) of
        Just (Controllable _) ->
            upsertComponent "controllable" (Controllable input) entity

        _ ->
            entity


applyInputGame : Game -> Game
applyInputGame game =
    { game
        | entities = List.map (applyInput game.input) game.entities
    }


onCollisionEnter : Time.Time -> Entity -> Entity
onCollisionEnter dt entity =
    case (getComponent "collisionEnter" entity) of
        Just (CollisionEnter enterFunc) ->
            enterFunc dt entity

        _ ->
            entity


checkCollision : Entity -> Entity -> Bool
checkCollision a b =
    let
        aComponents =
            ( getComponent "position" a, getComponent "boundingBox" a, getComponent "physics" a )

        bComponents =
            ( getComponent "position" b, getComponent "boundingBox" b )
    in
        if a.name == b.name then
            False
        else
            case aComponents of
                ( Just (Position ax ay), Just (BoundingBox aw ah), Just (Physics aPT cT) ) ->
                    if aPT == Static then
                        False
                    else
                        case bComponents of
                            ( Just (Position bx by), Just (BoundingBox bw bh) ) ->
                                if a.layer /= b.layer then
                                    False
                                else if
                                    (ay + ah)
                                        < by
                                        || ay
                                        > (by + bh)
                                        || (ax + aw)
                                        < bx
                                        || ax
                                        > (bx + bw)
                                then
                                    False
                                else
                                    True

                            _ ->
                                False

                _ ->
                    False


checkCollisions : List Entity -> Entity -> Entity
checkCollisions entities entity =
    case (getComponent "collision" entity) of
        Just (Collision objects) ->
            upsertComponent "collision" (Collision (List.filter (checkCollision entity) entities)) entity

        _ ->
            entity


sticky_threshold : Float
sticky_threshold =
    0.0004


resolveCollision : Entity -> Entity -> Entity
resolveCollision b a =
    if a.name == b.name then
        a
    else
        case ( getComponent "position" a, getComponent "velocity" a, getComponent "boundingBox" a, getComponent "physics" a ) of
            ( Just (Position ax ay), Just (Velocity avx avy), Just (BoundingBox aw ah), Just (Physics pT cT) ) ->
                if pT == Static then
                    a
                else
                    case ( getComponent "position" b, getComponent "boundingBox" b ) of
                        ( Just (Position bx by), Just (BoundingBox bw bh) ) ->
                            let
                                aMidX =
                                    ax + (aw / 2)

                                aMidY =
                                    ay + (ah / 2)

                                bMidX =
                                    bx + (bw / 2)

                                bMidY =
                                    by + (bh / 2)

                                dx =
                                    (bMidX - aMidX) / (bw / 2)

                                dy =
                                    (bMidY - aMidY) / (bh / 2)

                                absDx =
                                    abs dx

                                absDy =
                                    abs dy

                                coinFlip =
                                    ax > bx

                                newX =
                                    if dx < 0 then
                                        bx + bw + 0.01
                                    else
                                        bx - aw - 0.01

                                newY =
                                    if (dy < 0) then
                                        by + bh + 0.01
                                    else
                                        by - ah - 0.01

                                newVx =
                                    if (abs avx) < sticky_threshold then
                                        0
                                    else
                                        case cT of
                                            Displace ->
                                                0

                                            Elastic restitution ->
                                                case getComponent "velocity" b of
                                                    Just (Velocity bvx bvy) ->
                                                        (-avx * restitution) + (bvx * restitution)

                                                    _ ->
                                                        (-avx * restitution)

                                newVy =
                                    if (abs avy) < sticky_threshold then
                                        0
                                    else
                                        case cT of
                                            Displace ->
                                                0

                                            Elastic restitution ->
                                                case getComponent "velocity" b of
                                                    Just (Velocity bvx bvy) ->
                                                        (-avy * restitution) + (bvy * restitution)

                                                    _ ->
                                                        (-avy * restitution)
                            in
                                if (absDx > absDy) then
                                    a
                                        |> upsertComponent "position" (Position newX ay)
                                        |> upsertComponent "velocity" (Velocity newVx avy)
                                else
                                    a
                                        |> upsertComponent "position" (Position ax newY)
                                        |> upsertComponent "velocity" (Velocity avx newVy)

                        _ ->
                            a

            _ ->
                a


resolveCollisions : Entity -> Entity
resolveCollisions entity =
    case (getComponent "collision" entity) of
        Just (Collision entities) ->
            List.foldr (resolveCollision) entity entities

        _ ->
            entity



-- Components


removeComponent : String -> Entity -> Entity
removeComponent name ({ components } as entity) =
    { entity
        | components = Dict.remove name components
    }


upsertComponent : String -> Component -> Entity -> Entity
upsertComponent name component entity =
    let
        components =
            entity.components
    in
        { entity
            | components = Dict.insert name component components
        }


getComponent : String -> Entity -> Maybe Component
getComponent name entity =
    let
        toReturn =
            Dict.get name entity.components
    in
        toReturn



-- Utility


filterJusts : List (Maybe a) -> List a
filterJusts list =
    case list of
        [] ->
            []

        x :: xs ->
            case x of
                Nothing ->
                    filterJusts xs

                Just just ->
                    just :: filterJusts xs



-- Scenes


buildTestScene : Scene
buildTestScene =
    let
        mapLength =
            30

        mapHeight =
            30

        tileWidth =
            32

        bordersAndPlatform : Int -> Int -> Int
        bordersAndPlatform x y =
            if
                x
                    < 1
                    || x
                    > mapLength
                    - 2
                    || y
                    < 1
                    || y
                    >= mapHeight
                    - 2
            then
                1
            else if y == mapHeight // 2 && y > mapHeight // 2 then
                1
            else
                0

        tileAt : Int -> Maybe Entity
        tileAt tileType =
            if tileType == 1 then
                Just
                    ({ tag = "wall"
                     , name = "blueWall"
                     , layer = 0
                     , components =
                        Dict.fromList
                            [ ( "color", Color "rgba(0, 0, 255, 1)" )
                            , ( "shape", Shape (Square 1) )
                            , ( "boundingBox", BoundingBox 1 1 )
                            , ( "physics", Physics (Static) (Displace) )
                            ]
                     }
                    )
            else
                Nothing

        tiles =
            buildTileMapTiles mapLength mapHeight bordersAndPlatform

        tileMap =
            TileMap tiles tileAt mapLength mapHeight 0
    in
        Scene (mapLength * tileWidth) (mapHeight * tileWidth) tileWidth (Array.fromList [ tileMap ])


buildTileMapTiles : Int -> Int -> (Int -> Int -> Int) -> Array Int
buildTileMapTiles mapWidth mapHeight check =
    let
        bachedChecks : Int -> Int
        bachedChecks index =
            let
                x =
                    index % mapWidth

                y =
                    index // mapWidth
            in
                check x y
    in
        Array.initialize (mapWidth * mapHeight) bachedChecks
