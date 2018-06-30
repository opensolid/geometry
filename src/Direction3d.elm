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


module Direction3d
    exposing
        ( Direction3d
        , angleFrom
        , azimuth
        , componentIn
        , components
        , elevation
        , equalWithin
        , from
        , fromAzimuthAndElevation
        , mirrorAcross
        , negativeX
        , negativeY
        , negativeZ
        , on
        , orthogonalize
        , orthonormalize
        , perpendicularBasis
        , perpendicularTo
        , placeIn
        , positiveX
        , positiveY
        , positiveZ
        , projectInto
        , projectOnto
        , relativeTo
        , reverse
        , rotateAround
        , toVector
        , unsafe
        , x
        , xComponent
        , y
        , yComponent
        , z
        , zComponent
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Direction3d/icon.svg" alt="Direction3d" width="160">

A `Direction3d` represents a direction like 'up' or 'north' or 'forwards'. They
are represented using X, Y and Z components, and can be converted to vectors if
necessary, but should be thought of as conceptually different. Directions have
several uses, such as:

  - Constructing a vector from a length and direction
  - Determining the component of a vector in a particular direction (for
    example, finding the component of velocity in the up direction to get
    vertical speed)
  - Determining the angle between two directions
  - Defining the orientation of an axis, plane or reference frame

@docs Direction3d


# Constants

@docs x, y, z, positiveX, negativeX, positiveY, negativeY, positiveZ, negativeZ


# Constructors

@docs from, on, fromAzimuthAndElevation, perpendicularTo, perpendicularBasis, orthonormalize, orthogonalize, unsafe


# Properties

@docs components, xComponent, yComponent, zComponent, azimuth, elevation


# Comparison

@docs equalWithin


# Measurement

@docs componentIn, angleFrom


# Conversion

@docs toVector


# Transformations

@docs reverse, rotateAround, mirrorAcross, projectOnto


# Coordinate conversions

@docs relativeTo, placeIn, projectInto

-}

import Bootstrap.SketchPlane3d as SketchPlane3d
import Direction2d exposing (Direction2d)
import Geometry.Types as Types exposing (Axis3d, Frame3d, Plane3d, Point3d, SketchPlane3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


toDirection : Vector3d -> Direction3d
toDirection vector =
    unsafe (Vector3d.components vector)


{-| -}
type alias Direction3d =
    Types.Direction3d


{-| -}
x : Direction3d
x =
    unsafe ( 1, 0, 0 )


{-| -}
y : Direction3d
y =
    unsafe ( 0, 1, 0 )


{-| -}
z : Direction3d
z =
    unsafe ( 0, 0, 1 )


{-| -}
positiveX : Direction3d
positiveX =
    unsafe ( 1, 0, 0 )


{-| -}
negativeX : Direction3d
negativeX =
    unsafe ( -1, 0, 0 )


{-| -}
positiveY : Direction3d
positiveY =
    unsafe ( 0, 1, 0 )


{-| -}
negativeY : Direction3d
negativeY =
    unsafe ( 0, -1, 0 )


{-| -}
positiveZ : Direction3d
positiveZ =
    unsafe ( 0, 0, 1 )


{-| -}
negativeZ : Direction3d
negativeZ =
    unsafe ( 0, 0, -1 )


{-| -}
unsafe : ( Float, Float, Float ) -> Direction3d
unsafe =
    Types.Direction3d


{-| -}
on : SketchPlane3d -> Direction2d -> Direction3d
on sketchPlane direction2d =
    let
        ( dx, dy ) =
            Direction2d.components direction2d

        ( ux, uy, uz ) =
            components (SketchPlane3d.xDirection sketchPlane)

        ( vx, vy, vz ) =
            components (SketchPlane3d.yDirection sketchPlane)
    in
    unsafe
        ( dx * ux + dy * vx
        , dx * uy + dy * vy
        , dx * uz + dy * vz
        )


{-| -}
fromAzimuthAndElevation : Float -> Float -> Direction3d
fromAzimuthAndElevation azimuth_ elevation_ =
    let
        cosElevation =
            cos elevation_
    in
    unsafe
        ( cosElevation * cos azimuth_
        , cosElevation * sin azimuth_
        , sin elevation_
        )


{-| -}
from : Point3d -> Point3d -> Maybe Direction3d
from firstPoint secondPoint =
    Vector3d.direction (Vector3d.from firstPoint secondPoint)


{-| -}
perpendicularTo : Direction3d -> Direction3d
perpendicularTo direction =
    let
        perpendicularVector =
            Vector3d.perpendicularTo (toVector direction)

        length =
            Vector3d.length perpendicularVector

        normalizedVector =
            Vector3d.scaleBy (1 / length) perpendicularVector
    in
    toDirection normalizedVector


{-| -}
perpendicularBasis : Direction3d -> ( Direction3d, Direction3d )
perpendicularBasis direction =
    let
        xDirection =
            perpendicularTo direction

        yDirection =
            Vector3d.crossProduct (toVector direction) (toVector xDirection)
                |> toDirection
    in
    ( xDirection, yDirection )


{-| -}
orthonormalize : Vector3d -> Vector3d -> Vector3d -> Maybe ( Direction3d, Direction3d, Direction3d )
orthonormalize xVector xyVector xyzVector =
    Vector3d.direction xVector
        |> Maybe.andThen
            (\xDirection ->
                let
                    yVector =
                        Vector3d.crossProduct
                            (Vector3d.crossProduct xVector xyVector)
                            xVector
                in
                Vector3d.direction yVector
                    |> Maybe.andThen
                        (\yDirection ->
                            let
                                rightHandedZVector =
                                    Vector3d.crossProduct xVector yVector

                                dotProduct =
                                    Vector3d.dotProduct
                                        xyzVector
                                        rightHandedZVector

                                zVector =
                                    if dotProduct > 0 then
                                        rightHandedZVector
                                    else if dotProduct < 0 then
                                        Vector3d.reverse rightHandedZVector
                                    else
                                        Vector3d.zero
                            in
                            Vector3d.direction zVector
                                |> Maybe.map
                                    (\zDirection ->
                                        ( xDirection
                                        , yDirection
                                        , zDirection
                                        )
                                    )
                        )
            )


{-| -}
orthogonalize : Direction3d -> Direction3d -> Direction3d -> Maybe ( Direction3d, Direction3d, Direction3d )
orthogonalize xDirection yDirection zDirection =
    orthonormalize
        (toVector xDirection)
        (toVector yDirection)
        (toVector zDirection)


{-| -}
components : Direction3d -> ( Float, Float, Float )
components (Types.Direction3d components_) =
    components_


{-| -}
xComponent : Direction3d -> Float
xComponent (Types.Direction3d ( xComponent_, _, _ )) =
    xComponent_


{-| -}
yComponent : Direction3d -> Float
yComponent (Types.Direction3d ( _, yComponent_, _ )) =
    yComponent_


{-| -}
zComponent : Direction3d -> Float
zComponent (Types.Direction3d ( _, _, zComponent_ )) =
    zComponent_


{-| -}
componentIn : Direction3d -> Direction3d -> Float
componentIn firstDirection secondDirection =
    Vector3d.componentIn firstDirection (toVector secondDirection)


{-| -}
azimuth : Direction3d -> Float
azimuth direction =
    atan2 (yComponent direction) (xComponent direction)


{-| -}
elevation : Direction3d -> Float
elevation direction =
    asin (zComponent direction)


{-| -}
equalWithin : Float -> Direction3d -> Direction3d -> Bool
equalWithin angle firstDirection secondDirection =
    angleFrom firstDirection secondDirection <= angle


{-| -}
toVector : Direction3d -> Vector3d
toVector direction =
    Vector3d.fromComponents (components direction)


{-| -}
angleFrom : Direction3d -> Direction3d -> Float
angleFrom firstDirection secondDirection =
    let
        ( x1, y1, z1 ) =
            components firstDirection

        ( x2, y2, z2 ) =
            components secondDirection

        relativeX =
            x1 * x2 + y1 * y2 + z1 * z2

        cx =
            y1 * z2 - z1 * y2

        cy =
            z1 * x2 - x1 * z2

        cz =
            x1 * y2 - y1 * x2

        relativeY =
            sqrt (cx * cx + cy * cy + cz * cz)
    in
    atan2 relativeY relativeX


{-| -}
reverse : Direction3d -> Direction3d
reverse direction =
    let
        ( dx, dy, dz ) =
            components direction
    in
    unsafe ( -dx, -dy, -dz )


{-| -}
rotateAround : Axis3d -> Float -> Direction3d -> Direction3d
rotateAround axis angle direction =
    toVector direction |> Vector3d.rotateAround axis angle |> toDirection


{-| -}
mirrorAcross : Plane3d -> Direction3d -> Direction3d
mirrorAcross plane direction =
    toVector direction |> Vector3d.mirrorAcross plane |> toDirection


{-| -}
projectOnto : Plane3d -> Direction3d -> Maybe Direction3d
projectOnto plane direction =
    toVector direction |> Vector3d.projectOnto plane |> Vector3d.direction


{-| -}
relativeTo : Frame3d -> Direction3d -> Direction3d
relativeTo frame direction =
    toVector direction |> Vector3d.relativeTo frame |> toDirection


{-| -}
placeIn : Frame3d -> Direction3d -> Direction3d
placeIn frame direction =
    toVector direction |> Vector3d.placeIn frame |> toDirection


{-| -}
projectInto : SketchPlane3d -> Direction3d -> Maybe Direction2d
projectInto sketchPlane direction =
    toVector direction |> Vector3d.projectInto sketchPlane |> Vector2d.direction
