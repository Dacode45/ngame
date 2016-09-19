module Main exposing (..)

import List
import Keyboard
import Html exposing (..)
import Html.App
import Time exposing (Time)
import AnimationFrame


type alias Vector =
    { x : Float
    , y : Float
    }


gravity : Vector
gravity =
    { x = 0
    , y = -10
    }


type alias GObject =
    { position : Vector
    , velocity : Vector
    , forces : List Vector
    , isFloating : Bool
    , static : Bool
    }


type alias Model =
    { objs : List GObject
    }


model : Model
model =
    { objs = []
    }


addVector : Vector -> Vector -> Vector
addVector v1 v2 =
    { v1
        | x = v1.x + v2.x
        , y = v1.y + v2.y
    }


updateVector : Time -> Vector -> Vector -> Vector
updateVector dt v1 v2 =
    { v1
        | x = v1.x + (v2.x * dt)
        , y = v2.y + (v2.y * dt)
    }


applyPhysics : Time -> GObject -> GObject
applyPhysics dt obj =
    if obj.static then
        obj
    else
        { obj
            | position = updateVector dt obj.position obj.velocity
            , velocity = List.foldr (updateVector dt) obj.velocity obj.forces
            , forces = []
            , static = obj.static
        }


type Msg
    = TimeUpdate Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode


main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


view : Model -> Html msg
view model =
    div [] (model.objs |> List.map toString |> List.map text)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- case msg of =
--   TimeUpdate dt ->
--     ( { model |
--         objs = List.map (applyPhysics dt) model.objs
--       }, Cmd.none )
--   KeyDown keyCode ->
--     ( model, Cmd.none )
--   KeyUp keyCode ->
--     ( model, Cmd.none )


subscriptions : GObject -> Sub Msg
subscriptions obj =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
