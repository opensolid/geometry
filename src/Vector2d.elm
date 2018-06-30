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


module Vector2d
    exposing
        ( Vector2d
        , componentIn
        , components
        , crossProduct
        , difference
        , direction
        , dotProduct
        , equalWithin
        , from
        , fromComponents
        , fromPolarComponents
        , interpolateFrom
        , length
        , lengthAndDirection
        , mirrorAcross
        , normalize
        , perpendicularTo
        , placeIn
        , polarComponents
        , projectOnto
        , projectionIn
        , relativeTo
        , reverse
        , rotateBy
        , rotateClockwise
        , rotateCounterclockwise
        , scaleBy
        , squaredLength
        , sum
        , withLength
        , xComponent
        , yComponent
        , zero
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Vector2d/icon.svg" alt="Vector2d" width="160">

A `Vector2d` represents a quantity such as a displacement or velocity in 2D, and
is defined by its X and Y components. This module contains a variety of
vector-related functionality, such as

  - Adding or subtracting vectors
  - Finding the lengths of vectors
  - Rotating vectors
  - Converting vectors between different coordinate systems

Note that unlike in many other geometry packages where vectors are used as a
general-purpose data type, `opensolid/geometry` has separate data types for
vectors, directions and points. In most code it is actually more common to use
`Point2d` and `Direction2d` than `Vector2d`, and much code can avoid working
directly with `Vector2d` values at all!

@docs Vector2d


# Constants

@docs zero

Although there are no predefined constants for the vectors with components
(1,&nbsp;0) and (0,&nbsp;1), in most cases you will actually want their
`Direction2d` versions [`Direction2d.x`](Direction2d#x) and [`Direction2d.y`](Direction2d#y).


# Constructors

@docs fromComponents, fromPolarComponents, from, withLength, perpendicularTo, interpolateFrom


# Properties

@docs components, xComponent, yComponent, polarComponents, length, squaredLength, direction, lengthAndDirection


# Comparison

@docs equalWithin


# Measurement

@docs componentIn


# Arithmetic

@docs sum, difference, dotProduct, crossProduct


# Transformations

Note that for `mirrorAcross` and `projectOnto`, only the direction of the axis
affects the result, since vectors are position-independent. Think of
mirroring/projecting a vector across/onto an axis as moving the vector so its
tail is on the axis, then mirroring/projecting its tip across/onto the axis.

@docs reverse, normalize, scaleBy, rotateBy, rotateClockwise, rotateCounterclockwise, mirrorAcross, projectionIn, projectOnto


# Coordinate conversions

Like other transformations, coordinate conversions of vectors depend only on the
orientations of the relevant frames, not the positions of their origin points.

@docs relativeTo, placeIn

-}

import Bootstrap.Axis2d as Axis2d
import Bootstrap.Direction2d as Direction2d
import Bootstrap.Frame2d as Frame2d
import Bootstrap.Point2d as Point2d
import Float.Extra as Float
import Geometry.Types as Types exposing (Axis2d, Direction2d, Frame2d, Point2d)


{-| -}
type alias Vector2d =
    Types.Vector2d


{-| -}
zero : Vector2d
zero =
    fromComponents ( 0, 0 )


{-| -}
fromComponents : ( Float, Float ) -> Vector2d
fromComponents =
    Types.Vector2d


{-| -}
fromPolarComponents : ( Float, Float ) -> Vector2d
fromPolarComponents polarComponents_ =
    fromComponents (fromPolar polarComponents_)


{-| -}
from : Point2d -> Point2d -> Vector2d
from firstPoint secondPoint =
    let
        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint
    in
    fromComponents ( x2 - x1, y2 - y1 )


{-| -}
withLength : Float -> Direction2d -> Vector2d
withLength length_ direction_ =
    let
        ( dx, dy ) =
            Direction2d.components direction_
    in
    fromComponents ( length_ * dx, length_ * dy )


{-| -}
perpendicularTo : Vector2d -> Vector2d
perpendicularTo vector =
    rotateCounterclockwise vector


{-| -}
interpolateFrom : Vector2d -> Vector2d -> Float -> Vector2d
interpolateFrom v1 v2 t =
    let
        ( x1, y1 ) =
            components v1

        ( x2, y2 ) =
            components v2
    in
    fromComponents
        ( Float.interpolateFrom x1 x2 t
        , Float.interpolateFrom y1 y2 t
        )


{-| -}
components : Vector2d -> ( Float, Float )
components (Types.Vector2d components_) =
    components_


{-| -}
xComponent : Vector2d -> Float
xComponent (Types.Vector2d ( x, _ )) =
    x


{-| -}
yComponent : Vector2d -> Float
yComponent (Types.Vector2d ( _, y )) =
    y


{-| -}
componentIn : Direction2d -> Vector2d -> Float
componentIn direction_ vector =
    let
        ( dx, dy ) =
            Direction2d.components direction_

        ( vx, vy ) =
            components vector
    in
    vx * dx + vy * dy


{-| -}
polarComponents : Vector2d -> ( Float, Float )
polarComponents vector =
    toPolar (components vector)


{-| -}
equalWithin : Float -> Vector2d -> Vector2d -> Bool
equalWithin tolerance firstVector secondVector =
    squaredLength (difference firstVector secondVector) <= tolerance * tolerance


{-| -}
length : Vector2d -> Float
length vector =
    sqrt (squaredLength vector)


{-| -}
squaredLength : Vector2d -> Float
squaredLength vector =
    let
        ( x, y ) =
            components vector
    in
    x * x + y * y


{-| -}
direction : Vector2d -> Maybe Direction2d
direction vector =
    if vector == zero then
        Nothing
    else
        let
            normalizedVector =
                scaleBy (1 / length vector) vector
        in
        Just (Direction2d.unsafe (components normalizedVector))


{-| -}
lengthAndDirection : Vector2d -> Maybe ( Float, Direction2d )
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
                Direction2d.unsafe (components normalizedVector)
        in
        Just ( vectorLength, vectorDirection )


{-| -}
normalize : Vector2d -> Vector2d
normalize vector =
    if vector == zero then
        zero
    else
        scaleBy (1 / length vector) vector


{-| -}
sum : Vector2d -> Vector2d -> Vector2d
sum firstVector secondVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    fromComponents ( x1 + x2, y1 + y2 )


{-| -}
difference : Vector2d -> Vector2d -> Vector2d
difference firstVector secondVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    fromComponents ( x1 - x2, y1 - y2 )


{-| -}
dotProduct : Vector2d -> Vector2d -> Float
dotProduct firstVector secondVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    x1 * x2 + y1 * y2


{-| -}
crossProduct : Vector2d -> Vector2d -> Float
crossProduct firstVector secondVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    x1 * y2 - y1 * x2


{-| -}
reverse : Vector2d -> Vector2d
reverse vector =
    let
        ( x, y ) =
            components vector
    in
    fromComponents ( -x, -y )


{-| -}
scaleBy : Float -> Vector2d -> Vector2d
scaleBy scale vector =
    let
        ( x, y ) =
            components vector
    in
    fromComponents ( x * scale, y * scale )


{-| -}
rotateBy : Float -> Vector2d -> Vector2d
rotateBy angle =
    let
        cosine =
            cos angle

        sine =
            sin angle
    in
    \vector ->
        let
            ( x, y ) =
                components vector
        in
        fromComponents ( x * cosine - y * sine, y * cosine + x * sine )


{-| -}
rotateCounterclockwise : Vector2d -> Vector2d
rotateCounterclockwise vector =
    let
        ( x, y ) =
            components vector
    in
    fromComponents ( -y, x )


{-| -}
rotateClockwise : Vector2d -> Vector2d
rotateClockwise vector =
    let
        ( x, y ) =
            components vector
    in
    fromComponents ( y, -x )


{-| -}
mirrorAcross : Axis2d -> Vector2d -> Vector2d
mirrorAcross axis =
    let
        ( dx, dy ) =
            Direction2d.components (Axis2d.direction axis)

        a =
            1 - 2 * dy * dy

        b =
            2 * dx * dy

        c =
            1 - 2 * dx * dx
    in
    \vector ->
        let
            ( vx, vy ) =
                components vector
        in
        fromComponents ( a * vx + b * vy, c * vy + b * vx )


{-| -}
projectionIn : Direction2d -> Vector2d -> Vector2d
projectionIn direction_ vector =
    direction_ |> withLength (vector |> componentIn direction_)


{-| -}
projectOnto : Axis2d -> Vector2d -> Vector2d
projectOnto axis vector =
    projectionIn (Axis2d.direction axis) vector


{-| -}
relativeTo : Frame2d -> Vector2d -> Vector2d
relativeTo frame vector =
    fromComponents
        ( componentIn (Frame2d.xDirection frame) vector
        , componentIn (Frame2d.yDirection frame) vector
        )


{-| -}
placeIn : Frame2d -> Vector2d -> Vector2d
placeIn frame =
    let
        ( x1, y1 ) =
            Direction2d.components (Frame2d.xDirection frame)

        ( x2, y2 ) =
            Direction2d.components (Frame2d.yDirection frame)
    in
    \vector ->
        let
            ( x, y ) =
                components vector
        in
        fromComponents ( x1 * x + x2 * y, y1 * x + y2 * y )
