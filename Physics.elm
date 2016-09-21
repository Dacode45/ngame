module Physics exposing (..)


type PhysicsType
    = Static
    | Dynamic


type alias Vector =
    { x : Float
    , y : Float
    }


type alias RigidBody =
    { width : Float
    , height : Float
    , halfWidth : Float
    , halfHeight : Float
    , position : Vector
    , velocity : Vector
    , accelaration : Vector
    }


updateBounds : RigidBody -> RigidBody
updateBounds body =
    { body | halfHeight = body.height / 2, halfWidth = body.width / 2 }


getMidX : RigidBody -> Float
getMidX body =
    body.halfWidth + body.position.x


getMidY : RigidBody -> Float
getMidY body =
    body.halfHeight + body.position.y
