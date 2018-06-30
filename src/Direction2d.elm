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


module Direction2d
    exposing
        ( Direction2d
        , angleFrom
        , componentIn
        , components
        , equalWithin
        , from
        , fromAngle
        , mirrorAcross
        , negativeX
        , negativeY
        , orthogonalize
        , orthonormalize
        , perpendicularTo
        , placeIn
        , positiveX
        , positiveY
        , relativeTo
        , reverse
        , rotateBy
        , rotateClockwise
        , rotateCounterclockwise
        , toAngle
        , toVector
        , unsafe
        , x
        , xComponent
        , y
        , yComponent
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Direction2d/icon.svg" alt="Direction2d" width="160">

A `Direction2d` represents a direction like 'up' or 'north' or 'forwards'. They
are represented using X and Y components, and can be converted to vectors if
necessary, but should be thought of as conceptually different. Directions have
several uses, such as:

  - Constructing a vector from a length and direction
  - Determining the component of a vector in a particular direction (for
    example, finding the component of velocity in the up direction to get
    vertical speed)
  - Determining the (signed) angle between two directions
  - Defining the orientation of an axis or reference frame

@docs Direction2d


# Constants

@docs x, y, positiveX, negativeX, positiveY, negativeY


# Constructors

@docs from, perpendicularTo, orthonormalize, orthogonalize, unsafe


# Conversions

@docs fromAngle, toAngle


# Properties

@docs components, xComponent, yComponent


# Comparison

@docs equalWithin


# Measurement

@docs componentIn, angleFrom


# Conversion

@docs toVector


# Transformations

@docs reverse, rotateClockwise, rotateCounterclockwise, rotateBy, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Bootstrap.Direction2d as Bootstrap
import Geometry.Types as Types exposing (Axis2d, Frame2d, Point2d)
import Vector2d exposing (Vector2d)


toDirection : Vector2d -> Direction2d
toDirection vector =
    unsafe (Vector2d.components vector)


{-| -}
type alias Direction2d =
    Types.Direction2d


{-| -}
x : Direction2d
x =
    unsafe ( 1, 0 )


{-| -}
y : Direction2d
y =
    unsafe ( 0, 1 )


{-| -}
positiveX : Direction2d
positiveX =
    unsafe ( 1, 0 )


{-| -}
negativeX : Direction2d
negativeX =
    unsafe ( -1, 0 )


{-| -}
positiveY : Direction2d
positiveY =
    unsafe ( 0, 1 )


{-| -}
negativeY : Direction2d
negativeY =
    unsafe ( 0, -1 )


{-| -}
unsafe : ( Float, Float ) -> Direction2d
unsafe =
    Types.Direction2d


{-| -}
from : Point2d -> Point2d -> Maybe Direction2d
from firstPoint secondPoint =
    Vector2d.direction (Vector2d.from firstPoint secondPoint)


{-| -}
perpendicularTo : Direction2d -> Direction2d
perpendicularTo =
    Bootstrap.perpendicularTo


{-| -}
orthonormalize : Vector2d -> Vector2d -> Maybe ( Direction2d, Direction2d )
orthonormalize xVector xyVector =
    Vector2d.direction xVector
        |> Maybe.andThen
            (\xDirection ->
                let
                    yDirection =
                        perpendicularTo xDirection

                    perpendicularComponent =
                        Vector2d.componentIn yDirection xyVector
                in
                if perpendicularComponent > 0.0 then
                    Just ( xDirection, yDirection )
                else if perpendicularComponent < 0.0 then
                    Just ( xDirection, reverse yDirection )
                else
                    Nothing
            )


{-| -}
orthogonalize : Direction2d -> Direction2d -> Maybe ( Direction2d, Direction2d )
orthogonalize xDirection yDirection =
    orthonormalize (toVector xDirection) (toVector yDirection)


{-| -}
fromAngle : Float -> Direction2d
fromAngle angle =
    unsafe ( cos angle, sin angle )


{-| -}
toAngle : Direction2d -> Float
toAngle direction =
    let
        ( xComponent_, yComponent_ ) =
            components direction
    in
    atan2 yComponent_ xComponent_


{-| -}
angleFrom : Direction2d -> Direction2d -> Float
angleFrom firstDirection secondDirection =
    let
        firstVector =
            toVector firstDirection

        secondVector =
            toVector secondDirection
    in
    atan2 (Vector2d.crossProduct firstVector secondVector)
        (Vector2d.dotProduct firstVector secondVector)


{-| -}
components : Direction2d -> ( Float, Float )
components =
    Bootstrap.components


{-| -}
xComponent : Direction2d -> Float
xComponent (Types.Direction2d ( xComponent_, _ )) =
    xComponent_


{-| -}
yComponent : Direction2d -> Float
yComponent (Types.Direction2d ( _, yComponent_ )) =
    yComponent_


{-| -}
componentIn : Direction2d -> Direction2d -> Float
componentIn firstDirection secondDirection =
    Vector2d.componentIn firstDirection (toVector secondDirection)


{-| -}
equalWithin : Float -> Direction2d -> Direction2d -> Bool
equalWithin angle firstDirection secondDirection =
    abs (angleFrom firstDirection secondDirection) <= angle


{-| -}
toVector : Direction2d -> Vector2d
toVector direction =
    Vector2d.fromComponents (components direction)


{-| -}
reverse : Direction2d -> Direction2d
reverse =
    Bootstrap.reverse


{-| -}
rotateClockwise : Direction2d -> Direction2d
rotateClockwise direction =
    let
        ( xComponent_, yComponent_ ) =
            components direction
    in
    unsafe ( yComponent_, -xComponent_ )


{-| -}
rotateCounterclockwise : Direction2d -> Direction2d
rotateCounterclockwise direction =
    let
        ( xComponent_, yComponent_ ) =
            components direction
    in
    unsafe ( -yComponent_, xComponent_ )


{-| -}
rotateBy : Float -> Direction2d -> Direction2d
rotateBy angle direction =
    toVector direction |> Vector2d.rotateBy angle |> toDirection


{-| -}
mirrorAcross : Axis2d -> Direction2d -> Direction2d
mirrorAcross axis direction =
    toVector direction |> Vector2d.mirrorAcross axis |> toDirection


{-| -}
relativeTo : Frame2d -> Direction2d -> Direction2d
relativeTo frame direction =
    toVector direction |> Vector2d.relativeTo frame |> toDirection


{-| -}
placeIn : Frame2d -> Direction2d -> Direction2d
placeIn frame direction =
    toVector direction |> Vector2d.placeIn frame |> toDirection
