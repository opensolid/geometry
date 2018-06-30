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

Functions for transforming directions between local and global coordinates in
different coordinate frames. Like other transformations, coordinate
transformations of directions depend only on the orientations of the relevant
frames, not the positions of their origin points.

For the examples, assume the following frames have been defined:

    upsideDownFrame =
        Frame2d
            { originPoint = Point2d.origin
            , xDirection = Direction2d.positiveX
            , yDirection = Direction2d.negativeY
            }

    rotatedFrame =
        Frame2d.rotateBy (degrees 30) Frame2d.xy

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


{-| Synonym for `Direction2d.positiveX`.
-}
x : Direction2d
x =
    unsafe ( 1, 0 )


{-| Synonym for `Direction2d.positiveY`.
-}
y : Direction2d
y =
    unsafe ( 0, 1 )


{-| The positive X direction.

    Direction2d.components Direction2d.positiveX
    --> ( 1, 0 )

-}
positiveX : Direction2d
positiveX =
    unsafe ( 1, 0 )


{-| The negative X direction.

    Direction2d.components Direction2d.negativeX
    --> ( -1, 0 )

-}
negativeX : Direction2d
negativeX =
    unsafe ( -1, 0 )


{-| The positive Y direction.

    Direction2d.components Direction2d.positiveY
    --> ( 0, 1 )

-}
positiveY : Direction2d
positiveY =
    unsafe ( 0, 1 )


{-| The negative Y direction.

    Direction2d.components Direction2d.negativeY
    --> ( 0, -1 )

-}
negativeY : Direction2d
negativeY =
    unsafe ( 0, -1 )


{-| Construct a direction directly from its X and Y components. Note that **you
must ensure that the sum of the squares of the given components is exactly
one**:

    Direction2d.unsafe ( 1, 0 )
    Direction2d.unsafe ( 0, -1 )
    Direction2d.unsafe ( 0.6, 0.8 )

are all valid but

    Direction2d.unsafe ( 2, 0 )
    Direction2d.unsafe ( 1, 1 )

are not. Instead of using `Direction2d.unsafe`, it may be easier to use
constructors like `Direction2d.fromAngle` (which will always result in a valid
direction) or start with existing directions and transform them as necessary.

-}
unsafe : ( Float, Float ) -> Direction2d
unsafe =
    Types.Direction2d


{-| Attempt to construct the direction from the first given point to the second.
If the two points are coincident, returns `Nothing`.

    point =
        Point2d.fromCoordinates ( 1, 1 )

    Direction2d.from Point2d.origin point
    --> Just (Direction2d.fromAngle (degrees 45))

    Direction2d.from point Point2d.origin
    --> Just (Direction2d.fromAngle (degrees -135))

    Direction2d.from point point
    --> Nothing

-}
from : Point2d -> Point2d -> Maybe Direction2d
from firstPoint secondPoint =
    Vector2d.direction (Vector2d.from firstPoint secondPoint)


{-| Construct a direction perpendicular to the given direction, by rotating the
given direction 90 degrees counterclockwise. Synonym for
`rotateCounterclockwise`.

    Direction2d.perpendicularTo Direction2d.x
    --> Direction2d.y

    Direction2d.perpendicularTo Direction2d.y
    --> Direction2d.negativeX

-}
perpendicularTo : Direction2d -> Direction2d
perpendicularTo =
    Bootstrap.perpendicularTo


{-| Attempt to form a pair of perpendicular directions from the two given
vectors by performing [Gram-Schmidt normalization](https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process):

  - The first returned direction will be equal to the direction of the first
    given vector
  - The second returned direction will be as close as possible to the second
    given vector while being perpendicular to the first returned direction

If either of the given vectors are zero, or if the two vectors are parallel,
returns `Nothing`.

    Direction2d.orthonormalize
        (Vector2d.fromComponents ( 3, 3 ))
        (Vector2d.fromComponents ( 0, -2 ))
    --> Just
    -->     ( Direction2d.fromAngle (degrees 45)
    -->     , Direction2d.fromAngle (degrees -45)
    -->     )

    Direction2d.orthonormalize
        (Vector2d.fromComponents ( 3, 3 ))
        (Vector2d.fromComponents ( -2, -2 ))
    --> Nothing

-}
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


{-| Attempt to form a pair of perpendicular directions from the two given
directions;

    Direction2d.orthogonalize xDirection yDirection

is equivalent to

    Direction2d.orthonormalize
        (Direction2d.toVector xDirection)
        (Direction2d.toVector yDirection)

-}
orthogonalize : Direction2d -> Direction2d -> Maybe ( Direction2d, Direction2d )
orthogonalize xDirection yDirection =
    orthonormalize (toVector xDirection) (toVector yDirection)


{-| Construct a direction from an angle in radians, given counterclockwise from
the positive X direction.

    Direction2d.fromAngle 0
    --> Direction2d.x

    Direction2d.fromAngle (degrees 90)
    --> Direction2d.y

    Direction2d.fromAngle (degrees -90)
    --> Direction2d.negativeY

-}
fromAngle : Float -> Direction2d
fromAngle angle =
    unsafe ( cos angle, sin angle )


{-| Convert a direction to a polar angle (the counterclockwise angle in radians
from the positive X direction). The result will be in the range -π to π.

    Direction2d.toAngle Direction2d.x
    --> 0

    Direction2d.toAngle Direction2d.y
    --> degrees 90

    Direction2d.toAngle Direction2d.negativeY
    --> degrees -90

-}
toAngle : Direction2d -> Float
toAngle direction =
    let
        ( xComponent_, yComponent_ ) =
            components direction
    in
    atan2 yComponent_ xComponent_


{-| Find the counterclockwise angle in radians from the first direction to the
second. The result will be in the range -π to π.

    referenceDirection =
        Direction2d.fromAngle (degrees 30)

    Direction2d.angleFrom referenceDirection Direction2d.y
    --> degrees 60

    Direction2d.angleFrom referenceDirection Direction2d.x
    --> degrees -30

-}
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


{-| Get the components of a direction as a tuple (the components it would have
as a unit vector, also know as its direction cosines).

    ( x, y ) =
        Direction2d.components direction

-}
components : Direction2d -> ( Float, Float )
components =
    Bootstrap.components


{-| Get the X component of a direction.

    Direction2d.xComponent Direction2d.x
    --> 1

    Direction2d.xComponent Direction2d.y
    --> 0

-}
xComponent : Direction2d -> Float
xComponent (Types.Direction2d ( xComponent_, _ )) =
    xComponent_


{-| Get the Y component of a direction.

    Direction2d.yComponent Direction2d.x
    --> 0

    Direction2d.yComponent Direction2d.y
    --> 1

-}
yComponent : Direction2d -> Float
yComponent (Types.Direction2d ( _, yComponent_ )) =
    yComponent_


{-| Find the component of one direction in another direction. This is equal to
the cosine of the angle between the directions, or equivalently the dot product
of the two directions converted to unit vectors.

    direction =
        Direction2d.fromAngle (degrees 60)

    Direction2d.componentIn Direction2d.x direction
    --> 0.5

    Direction2d.componentIn direction direction
    --> 1

    Direction2d.componentIn Direction2d.x Direction2d.y
    --> 0

This is more general and flexible than using `xComponent` or `yComponent`, both
of which can be expressed in terms of `componentIn`; for example,

    Direction2d.xComponent direction

is equivalent to

    Direction2d.componentIn Direction2d.x direction

-}
componentIn : Direction2d -> Direction2d -> Float
componentIn firstDirection secondDirection =
    Vector2d.componentIn firstDirection (toVector secondDirection)


{-| Compare two directions within an angular tolerance. Returns true if the
absolute value of the angle between the two given directions is less than the
given tolerance.

    firstDirection =
        Direction2d.fromAngle (degrees 45)

    secondDirection =
        Direction2d.fromAngle (degrees 47)

    Direction2d.equalWithin (degrees 5)
        firstDirection
        secondDirection
    --> True

    Direction2d.equalWithin (degrees 1)
        firstDirection
        secondDirection
    --> False

-}
equalWithin : Float -> Direction2d -> Direction2d -> Bool
equalWithin angle firstDirection secondDirection =
    abs (angleFrom firstDirection secondDirection) <= angle


{-| Convert a direction to a unit vector.

    Direction2d.toVector Direction2d.x
    --> Vector2d.fromComponents ( 1, 0 )

-}
toVector : Direction2d -> Vector2d
toVector direction =
    Vector2d.fromComponents (components direction)


{-| Reverse a direction.

    Direction2d.reverse Direction2d.y
    --> Direction2d.negativeY

-}
reverse : Direction2d -> Direction2d
reverse =
    Bootstrap.reverse


{-| Rotate a direction by 90 degrees clockwise.

    Direction2d.rotateClockwise Direction2d.y
    --> Direction2d.x

    Direction2d.rotateClockwise Direction2d.x
    --> Direction2d.negativeY

-}
rotateClockwise : Direction2d -> Direction2d
rotateClockwise direction =
    let
        ( xComponent_, yComponent_ ) =
            components direction
    in
    unsafe ( yComponent_, -xComponent_ )


{-| Rotate a direction by 90 degrees counterclockwise.

    Direction2d.rotateClockwise Direction2d.x
    --> Direction2d.y

    Direction2d.rotateClockwise Direction2d.y
    --> Direction2d.negativeX

-}
rotateCounterclockwise : Direction2d -> Direction2d
rotateCounterclockwise direction =
    let
        ( xComponent_, yComponent_ ) =
            components direction
    in
    unsafe ( -yComponent_, xComponent_ )


{-| Rotate a direction counterclockwise by a given angle (in radians).

    Direction2d.rotateBy pi Direction2d.x
    --> Direction2d.negativeX

    Direction2d.rotateBy (degrees 45) Direction2d.y
    --> Direction2d.fromAngle (degrees 135)

-}
rotateBy : Float -> Direction2d -> Direction2d
rotateBy angle direction =
    toVector direction |> Vector2d.rotateBy angle |> toDirection


{-| Mirror a direction across a particular axis. Note that only the direction of
the axis affects the result, since directions are position-independent.

    slopedAxis =
        Axis2d.through
            (Point2d.fromCoordinates ( 100, 200 ))
            (Direction2d.fromAngle (degrees 45))

    Direction2d.mirrorAcross slopedAxis Direction2d.x
    --> Direction2d.y

    Direction2d.mirrorAcross slopedAxis Direction2d.y
    --> Direction2d.x

-}
mirrorAcross : Axis2d -> Direction2d -> Direction2d
mirrorAcross axis direction =
    toVector direction |> Vector2d.mirrorAcross axis |> toDirection


{-| Take a direction defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Direction2d.relativeTo upsideDownFrame Direction2d.y
    --> Direction2d.negativeY

    Direction2d.relativeTo rotatedFrame Direction2d.x
    --> Direction2d.fromAngle (degrees -30)

    Direction2d.relativeTo rotatedFrame Direction2d.y
    --> Direction2d.fromAngle (degrees 60)

-}
relativeTo : Frame2d -> Direction2d -> Direction2d
relativeTo frame direction =
    toVector direction |> Vector2d.relativeTo frame |> toDirection


{-| Take a direction defined in local coordinates relative to a given reference
frame, and return that direction expressed in global coordinates.

    Direction2d.placeIn upsideDownFrame Direction2d.y
    --> Direction2d.negativeY

    Direction2d.placeIn rotatedFrame Direction2d.x
    --> Direction2d.fromAngle (degrees 30)

    Direction2d.placeIn rotatedFrame Direction2d.y
    --> Direction2d.fromAngle (degrees 120)

-}
placeIn : Frame2d -> Direction2d -> Direction2d
placeIn frame direction =
    toVector direction |> Vector2d.placeIn frame |> toDirection
