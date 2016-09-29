module Physics exposing (..)

import Time exposing (Time)
import Random


type RigidbodyType
    = Kinematic
    | Dynamic
    | Static


type CollisionType
    = Ellastic
    | Displace


type alias Vector =
    { x : Float
    , y : Float
    }


sticky_threshold : Float
sticky_threshold =
    0.0004


type alias Rigidbody =
    { width : Float
    , height : Float
    , restitution : Float
    , rigidbodyType : RigidbodyType
    , collisionType : CollisionType
    , halfWidth : Float
    , halfHeight : Float
    , position : Vector
    , velocity : Vector
    , acceleration : Vector
    , constantForce : Vector
    }


updateBounds : Rigidbody -> Rigidbody
updateBounds body =
    { body | halfHeight = body.height / 2, halfWidth = body.width / 2 }


getMidX : Rigidbody -> Float
getMidX body =
    body.halfWidth + body.position.x


getMidY : Rigidbody -> Float
getMidY body =
    body.halfHeight + body.position.y


getTop : Rigidbody -> Float
getTop body =
    body.position.y


getLeft : Rigidbody -> Float
getLeft body =
    body.position.x


getBottom : Rigidbody -> Float
getBottom body =
    body.position.y + body.height


getRight : Rigidbody -> Float
getRight body =
    body.position.x + body.width



-- 1. User Interaction
-- 2. Positional Logic
-- 3. Detect Collisions
-- 4. Resolve Collisions


updatePhysics : Time -> List Rigidbody -> List Rigidbody
updatePhysics dt objs =
    objs


isColliding : Rigidbody -> Rigidbody -> Bool
isColliding a b =
    let
        al =
            getLeft a

        at =
            getTop a

        ar =
            getRight a

        ab =
            getBottom a

        bl =
            getLeft b

        bt =
            getTop b

        br =
            getRight b

        bb =
            getBottom b
    in
        if (ab < bt || at > bb || ar < bl || al > br) then
            False
        else
            True


resolveCollision : Rigidbody -> Rigidbody -> Rigidbody
resolveCollision a b =
    let
        aMidX =
            getMidX a

        aMidY =
            getMidY a

        bMidX =
            getMidX b

        bMidY =
            getMidY b

        dx =
            (bMidX - aMidX) / (b.halfWidth)

        dy =
            (bMidY - aMidY) / (b.halfHeight)

        absDX =
            abs dx

        absDY =
            abs dy

        aPos =
            a.position

        aVel =
            a.velocity

        bPos =
            b.position

        bVel =
            b.velocity

        updateX : Float
        updateX =
            if dx < 0 then
                --approach from the right
                getRight b
            else
                (getLeft b) - (a.width)

        reflectVx : Float
        reflectVx =
            if a.collisionType == Displace || (abs aVel.x) < sticky_threshold then
                0
            else
                -aVel.x * b.restitution

        updateY : Float
        updateY =
            if (dy < 0) then
                getBottom b
            else
                (getTop b) - (a.height)

        reflectVy : Float
        reflectVy =
            if a.collisionType == Displace || (abs aVel.y) < sticky_threshold then
                0
            else
                -aVel.y * b.restitution

        reflect : Vector
        reflect =
            let
                ( coinFlip, _ ) =
                    Random.step Random.bool (Random.initialSeed 123456)
            in
                if coinFlip then
                    { aVel | x = reflectVx }
                else
                    { aVel | y = reflectVy }
    in
        if a.rigidbodyType == Static || a == b then
            a
        else if ((abs (absDX - absDY)) < 0.1) then
            { a
                | position = Vector updateX updateY
                , velocity = reflect
            }
            -- object approaching from the side
        else if (absDX > absDY) then
            { a
                | position = Vector updateX a.position.y
                , velocity = Vector reflectVx a.velocity.y
            }
        else
            { a
                | position = Vector a.position.x updateY
                , velocity = Vector a.velocity.x reflectVy
            }


resolveCollisions : List Rigidbody -> Rigidbody -> Rigidbody
resolveCollisions bodies a =
    List.foldr (resolveCollision) a bodies


updateRigidbody : Time.Time -> Rigidbody -> Rigidbody
updateRigidbody dt body =
    let
        newPos =
            Vector (body.position.x + body.velocity.x) (body.position.y + body.velocity.y)

        newVel =
            Vector (body.velocity.x + ((body.acceleration.x + body.constantForce.x) * dt)) (body.velocity.y + ((body.acceleration.y + body.constantForce.y) * dt))
    in
        { body
            | position = newPos
            , velocity = newVel
            , acceleration = Vector 0 0
        }



-- Inplace collision Resolution


physicsStep : Time.Time -> List Rigidbody -> List Rigidbody
physicsStep dt bodies =
    let
        updatedBodies =
            List.map (updateRigidbody dt) bodies
    in
        List.map (resolveCollisions updatedBodies) updatedBodies
