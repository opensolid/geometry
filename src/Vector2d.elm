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

Functions for transforming vectors between local and global coordinates in
different coordinate frames. Like other transformations, coordinate conversions
of vectors depend only on the orientations of the relevant frames, not the
positions of their origin points.

For the examples, assume the following frame has been defined:

    rotatedFrame =
        Frame2d.rotateBy (degrees 30) Frame2d.xy

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


{-| The zero vector.

    Vector2d.zero
    --> Vector2d.fromComponents ( 0, 0 )

-}
zero : Vector2d
zero =
    fromComponents ( 0, 0 )


{-| Construct a vector from its X and Y components.

    vector =
        Vector2d.fromComponents ( 2, 3 )

-}
fromComponents : ( Float, Float ) -> Vector2d
fromComponents =
    Types.Vector2d


{-| Construct a vector from a length and angle. The angle is measured
counterclockwise from the positive X direction.

    Vector2d.fromPolarComponents ( 2, degrees 135 )
    -->Vector2d.fromComponents ( -1.4142, 1.4142 )

-}
fromPolarComponents : ( Float, Float ) -> Vector2d
fromPolarComponents polarComponents_ =
    fromComponents (fromPolar polarComponents_)


{-| Construct a vector from the first given point to the second.

    startPoint =
        Point2d.fromCoordinates ( 1, 1 )

    endPoint =
        Point2d.fromCoordinates ( 4, 5 )

    Vector2d.from startPoint endPoint
    --> Vector2d.fromComponents ( 3, 4 )

-}
from : Point2d -> Point2d -> Vector2d
from firstPoint secondPoint =
    let
        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint
    in
    fromComponents ( x2 - x1, y2 - y1 )


{-| Construct a vector with the given length in the given direction.

    Vector2d.withLength 5 Direction2d.y
    --> Vector2d.fromComponents ( 0, 5 )

-}
withLength : Float -> Direction2d -> Vector2d
withLength length_ direction_ =
    let
        ( dx, dy ) =
            Direction2d.components direction_
    in
    fromComponents ( length_ * dx, length_ * dy )


{-| Construct a vector perpendicular to the given vector, by rotating the given
vector 90 degrees counterclockwise. The constructed vector will have the same
length as the given vector. Alias for `Vector2d.rotateCounterclockwise`.

    Vector2d.perpendicularTo
        (Vector2d.fromComponents ( 1, 0 ))
    --> Vector2d.fromComponents ( 0, 1 )

    Vector2d.perpendicularTo
        (Vector2d.fromComponents ( 0, 2 ))
    --> Vector2d.fromComponents ( -2, 0 )

    Vector2d.perpendicularTo
        (Vector2d.fromComponents ( 3, 1 ))
    --> Vector2d.fromComponents ( -1, 3 )

    Vector2d.perpendicularTo Vector2d.zero
    --> Vector2d.zero

-}
perpendicularTo : Vector2d -> Vector2d
perpendicularTo vector =
    rotateCounterclockwise vector


{-| Construct a vector by interpolating from the first given vector to the
second, based on a parameter that ranges from zero to one.

    startVector =
        Vector2d.zero

    endVector =
        Vector2d.fromComponents ( 8, 12 )

    Vector2d.interpolateFrom startVector endVector 0.25
    --> Vector2d.fromComponents ( 2, 3 )

Partial application may be useful:

    interpolatedVector : Float -> Vector2d
    interpolatedVector =
        Vector2d.interpolateFrom startVector endVector

    List.map interpolatedVector [ 0, 0.5, 1 ]
    --> [ Vector2d.fromComponents ( 0, 0 )
    --> , Vector2d.fromComponents ( 4, 6 )
    --> , Vector2d.fromComponents ( 8, 12 )
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedVector -0.5
    --> Vector2d.fromComponents ( -4, -6 )

    interpolatedVector 1.25
    --> Vector2d.fromComponents ( 10, 15 )

-}
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


{-| Extract the components of a vector.

    Vector2d.components (Vector2d.fromComponents ( 2, 3 ))
    --> ( 2, 3 )

This combined with Elm's built-in tuple destructuring provides a convenient way
to extract both the X and Y components of a vector in one line of code:

    ( x, y ) =
        Vector2d.components vector

-}
components : Vector2d -> ( Float, Float )
components (Types.Vector2d components_) =
    components_


{-| Get the X component of a vector.

    Vector2d.xComponent (Vector2d.fromComponents ( 2, 3 ))
    --> 2

-}
xComponent : Vector2d -> Float
xComponent (Types.Vector2d ( x, _ )) =
    x


{-| Get the Y component of a vector.

    Vector2d.yComponent (Vector2d.fromComponents ( 2, 3 ))
    --> 3

-}
yComponent : Vector2d -> Float
yComponent (Types.Vector2d ( _, y )) =
    y


{-| Find the component of a vector in an arbitrary direction, for example

    forwardSpeed =
        Vector2d.componentIn forwardDirection velocity

This is more general and flexible than using `xComponent` or `yComponent`, both
of which can be expressed in terms of `componentIn`; for example,

    Vector2d.xComponent vector

is equivalent to

    Vector2d.componentIn Direction2d.x vector

-}
componentIn : Direction2d -> Vector2d -> Float
componentIn direction_ vector =
    let
        ( dx, dy ) =
            Direction2d.components direction_

        ( vx, vy ) =
            components vector
    in
    vx * dx + vy * dy


{-| Get the polar components (length, polar angle) of a vector.

    Vector2d.polarComponents
        (Vector2d.fromComponents ( 1, 1 ))
    --> ( 1.4142, degrees 45 )

-}
polarComponents : Vector2d -> ( Float, Float )
polarComponents vector =
    toPolar (components vector)


{-| Compare two vectors within a tolerance. Returns true if the difference
between the two given vectors has magnitude less than the given tolerance.

    firstVector =
        Vector2d.fromComponents ( 1, 2 )

    secondVector =
        Vector2d.fromComponents ( 0.9999, 2.0002 )

    Vector2d.equalWithin 1e-3 firstVector secondVector
    --> True

    Vector2d.equalWithin 1e-6 firstVector secondVector
    --> False

-}
equalWithin : Float -> Vector2d -> Vector2d -> Bool
equalWithin tolerance firstVector secondVector =
    squaredLength (difference firstVector secondVector) <= tolerance * tolerance


{-| Get the length (magnitude) of a vector.

    Vector2d.length (Vector2d.fromComponents ( 3, 4 ))
    --> 5

-}
length : Vector2d -> Float
length vector =
    sqrt (squaredLength vector)


{-| Get the squared length of a vector. `squaredLength` is slightly faster than
`length`, so for example

    Vector2d.squaredLength vector > tolerance * tolerance

is equivalent to but slightly more efficient than

    Vector2d.length vector > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `length` is much more
readable!

-}
squaredLength : Vector2d -> Float
squaredLength vector =
    let
        ( x, y ) =
            components vector
    in
    x * x + y * y


{-| Attempt to find the direction of a vector. In the case of a zero vector,
return `Nothing`.

    Vector2d.direction (Vector2d.fromComponents ( 3, 3 ))
    --> Just (Direction2d.fromAngle (degrees 45))

    Vector2d.direction Vector2d.zero
    --> Nothing

-}
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


{-| Attempt to find the length and direction of a vector. In the case of a zero
vector, returns `Nothing`.

    vector =
        Vector2d.fromComponents ( 1, 1 )

    Vector2d.lengthAndDirection vector
    --> Just
    -->     ( 1.4142
    -->     , Direction2d.fromAngle (degrees 45)
    -->     )

    Vector2d.lengthAndDirection Vector2d.zero
    --> Nothing

-}
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


{-| Normalize a vector to have a length of one. Zero vectors are left as-is.

    vector =
        Vector2d.fromComponents ( 3, 4 )

    Vector2d.normalize vector
    --> Vector2d.fromComponents ( 0.6, 0.8 )

    Vector2d.normalize Vector2d.zero
    --> Vector2d.zero

**Warning**: `Vector2d.direction` is safer since it forces you to explicitly
consider the case where the given vector is zero. `Vector2d.normalize` is
primarily useful for cases like generating WebGL meshes, where defaulting to a
zero vector for degenerate cases is acceptable, and the overhead of something
like

    Vector2d.direction vector
        |> Maybe.map Direction2d.toVector
        |> Maybe.withDefault Vector2d.zero

(which is functionally equivalent to `Vector2d.normalize vector`) is too high.

-}
normalize : Vector2d -> Vector2d
normalize vector =
    if vector == zero then
        zero
    else
        scaleBy (1 / length vector) vector


{-| Find the sum of two vectors.

    firstVector =
        Vector2d.fromComponents ( 1, 2 )

    secondVector =
        Vector2d.fromComponents ( 3, 4 )

    Vector2d.sum firstVector secondVector
    --> Vector2d.fromComponents ( 4, 6 )

-}
sum : Vector2d -> Vector2d -> Vector2d
sum firstVector secondVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    fromComponents ( x1 + x2, y1 + y2 )


{-| Find the difference between two vectors (the first vector minus the second).

    firstVector =
        Vector2d.fromComponents ( 5, 6 )

    secondVector =
        Vector2d.fromComponents ( 1, 3 )

    Vector2d.difference firstVector secondVector
    --> Vector2d.fromComponents ( 4, 3 )

-}
difference : Vector2d -> Vector2d -> Vector2d
difference firstVector secondVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    fromComponents ( x1 - x2, y1 - y2 )


{-| Find the dot product of two vectors.

    firstVector =
        Vector2d.fromComponents ( 1, 2 )

    secondVector =
        Vector2d.fromComponents ( 3, 4 )

    Vector2d.dotProduct firstVector secondVector
    --> 11

-}
dotProduct : Vector2d -> Vector2d -> Float
dotProduct firstVector secondVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    x1 * x2 + y1 * y2


{-| Find the scalar 'cross product' of two vectors in 2D. This is defined as

    crossProduct firstVector secondVector =
        let
            ( x1, y1 ) =
                components firstVector

            ( x2, y2 ) =
                components secondVector
        in
        x1 * y2 - y1 * x2

and is useful in many of the same ways as the 3D cross product:

  - Its length is equal to the product of the lengths of the two given vectors
    and the sine of the angle between them, so it can be used as a metric to
    determine if two vectors are nearly parallel.
  - The sign of the result indicates the direction of rotation from the first
    vector to the second (positive indicates a counterclockwise rotation and
    negative indicates a clockwise rotation), similar to how the direction of
    the 3D cross product indicates the direction of rotation.

Some examples:

    firstVector =
        Vector2d.fromComponents ( 2, 0 )

    secondVector =
        Vector2d.fromComponents ( 0, 3 )

    Vector2d.crossProduct firstVector secondVector
    --> 6

    Vector2d.crossProduct secondVector firstVector
    --> -6

    Vector2d.crossProduct firstVector firstVector
    --> 0

-}
crossProduct : Vector2d -> Vector2d -> Float
crossProduct firstVector secondVector =
    let
        ( x1, y1 ) =
            components firstVector

        ( x2, y2 ) =
            components secondVector
    in
    x1 * y2 - y1 * x2


{-| Reverse the direction of a vector, negating its components.

    Vector2d.reverse (Vector2d.fromComponents ( -1, 2 ))
    --> Vector2d.fromComponents ( 1, -2 )

-}
reverse : Vector2d -> Vector2d
reverse vector =
    let
        ( x, y ) =
            components vector
    in
    fromComponents ( -x, -y )


{-| Scale the length of a vector by a given scale.

    Vector2d.scaleBy 3 (Vector2d.fromComponents ( 1, 2 ))
    --> Vector2d.fromComponents ( 3, 6 )

-}
scaleBy : Float -> Vector2d -> Vector2d
scaleBy scale vector =
    let
        ( x, y ) =
            components vector
    in
    fromComponents ( x * scale, y * scale )


{-| Rotate a vector counterclockwise by a given angle (in radians).

    Vector2d.fromComponents ( 1, 1 )
        |> Vector2d.rotateBy (degrees 45)
    --> Vector2d.fromComponents ( 0, 1.4142 )

    Vector2d.fromComponents ( 1, 0 )
        |> Vector2d.rotateBy pi
    --> Vector2d.fromComponents ( -1, 0 )

-}
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


{-| Rotate the given vector 90 degrees counterclockwise;

    Vector2d.rotateCounterclockwise vector

is equivalent to

    Vector2d.rotateBy (degrees 90) vector

but is more efficient.

-}
rotateCounterclockwise : Vector2d -> Vector2d
rotateCounterclockwise vector =
    let
        ( x, y ) =
            components vector
    in
    fromComponents ( -y, x )


{-| Rotate the given vector 90 degrees clockwise;

    Vector2d.rotateClockwise vector

is equivalent to

    Vector2d.rotateBy (degrees -90) vector

but is more efficient.

-}
rotateClockwise : Vector2d -> Vector2d
rotateClockwise vector =
    let
        ( x, y ) =
            components vector
    in
    fromComponents ( y, -x )


{-| Mirror a vector across a given axis.

    vector =
        Vector2d.fromComponents ( 2, 3 )

    Vector2d.mirrorAcross Axis2d.y vector
    --> Vector2d.fromComponents ( -2, 3 )

The position of the axis doesn't matter, only its orientation:

    horizontalAxis =
        Axis2d.withDirection Direction2d.x
            (Point2d.fromCoordinates ( 100, 200 ))

    Vector2d.mirrorAcross horizontalAxis vector
    --> Vector2d.fromComponents ( 2, -3 )

-}
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


{-| Find the projection of a vector in a particular direction. Conceptually,
this means splitting the original vector into a portion parallel to the given
direction and a portion perpendicular to it, then returning the parallel
portion.

    vector =
        Vector2d.fromComponents ( 2, 3 )

    Vector2d.projectionIn Direction2d.x vector
    --> Vector2d.fromComponents ( 2, 0 )

    Vector2d.projectionIn Direction2d.y vector
    --> Vector2d.fromComponents ( 0, 3 )

-}
projectionIn : Direction2d -> Vector2d -> Vector2d
projectionIn direction_ vector =
    direction_ |> withLength (vector |> componentIn direction_)


{-| Project a vector onto an axis.

    Vector2d.projectOnto Axis2d.y
        (Vector2d.fromComponents ( 3, 4 ))
    --> Vector2d.fromComponents ( 0, 4 )

    Vector2d.projectOnto Axis2d.x
        (Vector2d.fromComponents ( -1, 2 ))
    --> Vector2d.fromComponents ( -1, 0 )

This is equivalent to finding the projection in the axis' direction.

-}
projectOnto : Axis2d -> Vector2d -> Vector2d
projectOnto axis vector =
    projectionIn (Axis2d.direction axis) vector


{-| Take a vector defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Vector2d.fromComponents ( 2, 0 )
        |> Vector2d.relativeTo rotatedFrame
    --> Vector2d.fromComponents ( 1.732, -1 )

-}
relativeTo : Frame2d -> Vector2d -> Vector2d
relativeTo frame vector =
    fromComponents
        ( componentIn (Frame2d.xDirection frame) vector
        , componentIn (Frame2d.yDirection frame) vector
        )


{-| Take a vector defined in local coordinates relative to a given reference
frame, and return that vector expressed in global coordinates.

    Vector2d.fromComponents ( 2, 0 )
        |> Vector2d.placeIn rotatedFrame
    --> Vector2d.fromComponents ( 1.732, 1 )

-}
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
