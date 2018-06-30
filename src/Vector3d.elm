--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Vector3d
    exposing
        ( Vector3d
        , componentIn
        , components
        , crossProduct
        , difference
        , direction
        , dotProduct
        , equalWithin
        , from
        , fromComponents
        , interpolateFrom
        , length
        , lengthAndDirection
        , mirrorAcross
        , normalize
        , on
        , perpendicularTo
        , placeIn
        , projectInto
        , projectOnto
        , projectionIn
        , relativeTo
        , reverse
        , rotateAround
        , scaleBy
        , squaredLength
        , sum
        , withLength
        , xComponent
        , yComponent
        , zComponent
        , zero
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Vector3d/icon.svg" alt="Vector3d" width="160">

A `Vector3d` represents a quantity such as a displacement or velocity in 3D, and
is defined by its X, Y and Z components. This module contains a variety of
vector-related functionality, such as

  - Adding or subtracting vectors
  - Finding the lengths of vectors
  - Rotating vectors
  - Converting vectors between different coordinate systems

Note that unlike in many other geometry packages where vectors are used as a
general-purpose data type, `opensolid/geometry` has separate data types for
vectors, directions and points. In most code it is actually more common to use
`Point3d` and `Direction3d` than `Vector3d`, and much code can avoid working
directly with `Vector3d` values at all!

@docs Vector3d


# Predefined vectors

@docs zero

Although there are no predefined constants for the vectors with components
(1,&nbsp;0,&nbsp;0), (0,&nbsp;1,&nbsp;0) and (0,&nbsp;0,&nbsp;1), in most cases
you will actually want their `Direction3d` versions [`Direction3d.x`](Direction3d#x),
[`Direction3d.y`](Direction3d#y) and [`Direction3d.z`](Direction3d#z).


# Constructors

@docs fromComponents, from, withLength, on, perpendicularTo, interpolateFrom


# Components

@docs components, xComponent, yComponent, zComponent, length, squaredLength, direction, lengthAndDirection


# Comparison

@docs equalWithin


# Measurement

@docs componentIn


# Arithmetic

@docs sum, difference, dotProduct, crossProduct


# Transformations

Note that for all transformations, only the orientation of the given axis or
plane is relevant, since vectors are position-independent. Think of transforming
a vector as placing its tail on the relevant axis or plane and then transforming
its tip.

@docs reverse, normalize, scaleBy, rotateAround, mirrorAcross, projectionIn, projectOnto


# Coordinate conversions

Like other transformations, coordinate transformations of vectors depend only on
the orientations of the relevant frames/sketch planes, not their positions.

@docs relativeTo, placeIn, projectInto

-}

import Bootstrap.Direction3d as Direction3d
import Bootstrap.Frame3d as Frame3d
import Bootstrap.Plane3d as Plane3d
import Bootstrap.Point3d as Point3d
import Bootstrap.SketchPlane3d as SketchPlane3d
import Float.Extra as Float
import Geometry.Types as Types exposing (Axis3d, Direction3d, Frame3d, Plane3d, Point3d, SketchPlane3d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Vector3d =
    Types.Vector3d


{-| -}
zero : Vector3d
zero =
    fromComponents ( 0, 0, 0 )


{-| -}
fromComponents : ( Float, Float, Float ) -> Vector3d
fromComponents =
    Types.Vector3d


{-| -}
from : Point3d -> Point3d -> Vector3d
from firstPoint secondPoint =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates firstPoint

        ( x2, y2, z2 ) =
            Point3d.coordinates secondPoint
    in
    fromComponents ( x2 - x1, y2 - y1, z2 - z1 )


{-| -}
withLength : Float -> Direction3d -> Vector3d
withLength length_ direction_ =
    let
        ( dx, dy, dz ) =
            Direction3d.components direction_
    in
    fromComponents ( length_ * dx, length_ * dy, length_ * dz )


{-| -}
on : SketchPlane3d -> Vector2d -> Vector3d
on sketchPlane vector2d =
    let
        ( ux, uy, uz ) =
            Direction3d.components (SketchPlane3d.xDirection sketchPlane)

        ( vx, vy, vz ) =
            Direction3d.components (SketchPlane3d.yDirection sketchPlane)

        ( x, y ) =
            Vector2d.components vector2d
    in
    fromComponents
        ( x * ux + y * vx
        , x * uy + y * vy
        , x * uz + y * vz
        )


{-| -}
perpendicularTo : Vector3d -> Vector3d
perpendicularTo vector =
    let
        ( x, y, z ) =
            components vector

        absX =
            abs x

        absY =
            abs y

        absZ =
            abs z
    in
    if absX <= absY then
        if absX <= absZ then
            fromComponents ( 0, -z, y )
        else
            fromComponents ( -y, x, 0 )
    else if absY <= absZ then
        fromComponents ( z, 0, -x )
    else
        fromComponents ( -y, x, 0 )


{-| -}
interpolateFrom : Vector3d -> Vector3d -> Float -> Vector3d
interpolateFrom v1 v2 t =
    let
        ( x1, y1, z1 ) =
            components v1

        ( x2, y2, z2 ) =
            components v2
    in
    fromComponents
        ( Float.interpolateFrom x1 x2 t
        , Float.interpolateFrom y1 y2 t
        , Float.interpolateFrom z1 z2 t
        )


{-| -}
components : Vector3d -> ( Float, Float, Float )
components (Types.Vector3d components_) =
    components_


{-| -}
xComponent : Vector3d -> Float
xComponent (Types.Vector3d ( x, _, _ )) =
    x


{-| -}
yComponent : Vector3d -> Float
yComponent (Types.Vector3d ( _, y, _ )) =
    y


{-| -}
zComponent : Vector3d -> Float
zComponent (Types.Vector3d ( _, _, z )) =
    z


{-| -}
componentIn : Direction3d -> Vector3d -> Float
componentIn direction_ vector =
    let
        ( dx, dy, dz ) =
            Direction3d.components direction_

        ( vx, vy, vz ) =
            components vector
    in
    vx * dx + vy * dy + vz * dz


{-| -}
equalWithin : Float -> Vector3d -> Vector3d -> Bool
equalWithin tolerance firstVector secondVector =
    squaredLength (difference firstVector secondVector) <= tolerance * tolerance


{-| -}
length : Vector3d -> Float
length vector =
    sqrt (squaredLength vector)


{-| -}
squaredLength : Vector3d -> Float
squaredLength vector =
    let
        ( x, y, z ) =
            components vector
    in
    x * x + y * y + z * z


{-| -}
direction : Vector3d -> Maybe Direction3d
direction vector =
    if vector == zero then
        Nothing
    else
        let
            normalizedVector =
                scaleBy (1 / length vector) vector
        in
        Just (Direction3d.unsafe (components normalizedVector))


{-| -}
lengthAndDirection : Vector3d -> Maybe ( Float, Direction3d )
lengthAndDirection vector =
    let
        vectorLength =
            length vector
    in
    if vectorLength == 0.0 then
        Nothing
    else
        let
            normalizedVector =
                scaleBy (1 / vectorLength) vector

            vectorDirection =
                Direction3d.unsafe (components normalizedVector)
        in
        Just ( vectorLength, vectorDirection )


{-| -}
normalize : Vector3d -> Vector3d
normalize vector =
    if vector == zero then
        zero
    else
        scaleBy (1 / length vector) vector


{-| -}
sum : Vector3d -> Vector3d -> Vector3d
sum firstVector secondVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    fromComponents ( x1 + x2, y1 + y2, z1 + z2 )


{-| -}
difference : Vector3d -> Vector3d -> Vector3d
difference firstVector secondVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    fromComponents ( x1 - x2, y1 - y2, z1 - z2 )


{-| -}
dotProduct : Vector3d -> Vector3d -> Float
dotProduct firstVector secondVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    x1 * x2 + y1 * y2 + z1 * z2


{-| -}
crossProduct : Vector3d -> Vector3d -> Vector3d
crossProduct firstVector secondVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    fromComponents
        ( y1 * z2 - z1 * y2
        , z1 * x2 - x1 * z2
        , x1 * y2 - y1 * x2
        )


{-| -}
reverse : Vector3d -> Vector3d
reverse vector =
    let
        ( x, y, z ) =
            components vector
    in
    fromComponents ( -x, -y, -z )


{-| -}
scaleBy : Float -> Vector3d -> Vector3d
scaleBy scale vector =
    let
        ( x, y, z ) =
            components vector
    in
    fromComponents ( x * scale, y * scale, z * scale )


{-| -}
rotateAround : Axis3d -> Float -> Vector3d -> Vector3d
rotateAround (Types.Axis3d axis) angle =
    let
        ( dx, dy, dz ) =
            Direction3d.components axis.direction

        halfAngle =
            0.5 * angle

        sinHalfAngle =
            sin halfAngle

        qx =
            dx * sinHalfAngle

        qy =
            dy * sinHalfAngle

        qz =
            dz * sinHalfAngle

        qw =
            cos halfAngle

        wx =
            qw * qx

        wy =
            qw * qy

        wz =
            qw * qz

        xx =
            qx * qx

        xy =
            qx * qy

        xz =
            qx * qz

        yy =
            qy * qy

        yz =
            qy * qz

        zz =
            qz * qz

        a00 =
            1 - 2 * (yy + zz)

        a10 =
            2 * (xy + wz)

        a20 =
            2 * (xz - wy)

        a01 =
            2 * (xy - wz)

        a11 =
            1 - 2 * (xx + zz)

        a21 =
            2 * (yz + wx)

        a02 =
            2 * (xz + wy)

        a12 =
            2 * (yz - wx)

        a22 =
            1 - 2 * (xx + yy)
    in
    \vector ->
        let
            ( x, y, z ) =
                components vector
        in
        fromComponents
            ( a00 * x + a01 * y + a02 * z
            , a10 * x + a11 * y + a12 * z
            , a20 * x + a21 * y + a22 * z
            )


{-| -}
mirrorAcross : Plane3d -> Vector3d -> Vector3d
mirrorAcross plane =
    let
        ( dx, dy, dz ) =
            Direction3d.components (Plane3d.normalDirection plane)

        a =
            1 - 2 * dx * dx

        b =
            1 - 2 * dy * dy

        c =
            1 - 2 * dz * dz

        d =
            -2 * dy * dz

        e =
            -2 * dx * dz

        f =
            -2 * dx * dy
    in
    \vector ->
        let
            ( x, y, z ) =
                components vector
        in
        fromComponents
            ( a * x + f * y + e * z
            , f * x + b * y + d * z
            , e * x + d * y + c * z
            )


{-| -}
projectionIn : Direction3d -> Vector3d -> Vector3d
projectionIn direction_ vector =
    direction_ |> withLength (vector |> componentIn direction_)


{-| -}
projectOnto : Plane3d -> Vector3d -> Vector3d
projectOnto plane vector =
    difference vector (projectionIn (Plane3d.normalDirection plane) vector)


{-| -}
relativeTo : Frame3d -> Vector3d -> Vector3d
relativeTo frame vector =
    fromComponents
        ( componentIn (Frame3d.xDirection frame) vector
        , componentIn (Frame3d.yDirection frame) vector
        , componentIn (Frame3d.zDirection frame) vector
        )


{-| -}
placeIn : Frame3d -> Vector3d -> Vector3d
placeIn frame vector =
    let
        ( x1, y1, z1 ) =
            Direction3d.components (Frame3d.xDirection frame)

        ( x2, y2, z2 ) =
            Direction3d.components (Frame3d.yDirection frame)

        ( x3, y3, z3 ) =
            Direction3d.components (Frame3d.zDirection frame)

        ( x, y, z ) =
            components vector
    in
    fromComponents
        ( x1 * x + x2 * y + x3 * z
        , y1 * x + y2 * y + y3 * z
        , z1 * x + z2 * y + z3 * z
        )


{-| -}
projectInto : SketchPlane3d -> Vector3d -> Vector2d
projectInto sketchPlane vector =
    Vector2d.fromComponents
        ( componentIn (SketchPlane3d.xDirection sketchPlane) vector
        , componentIn (SketchPlane3d.yDirection sketchPlane) vector
        )
