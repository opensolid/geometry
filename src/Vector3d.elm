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
general-purpose data type, `elm-geometry` has separate data types for vectors,
directions and points. In most code it is actually more common to use `Point3d`
and `Direction3d` than `Vector3d`, and much code can avoid working directly with
`Vector3d` values at all!

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

Functions for transforming vectors between local and global coordinates in
different coordinate frames. Like other transformations, coordinate
transformations of vectors depend only on the orientations of the relevant
frames/sketch planes, not their positions.

For the examples, assume the following definition of a local coordinate frame,
one that is rotated 30 degrees counterclockwise around the Z axis from the
global XYZ frame:

    rotatedFrame =
        Frame3d.rotateAround Axis3d.z (degrees 30) Frame3d.xyz

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


{-| The zero vector.

    Vector3d.zero
    --> Vector3d.fromComponents ( 0, 0, 0 )

-}
zero : Vector3d
zero =
    fromComponents ( 0, 0, 0 )


{-| Construct a vector from its X, Y and Z components.

    vector =
        Vector3d.fromComponents ( 2, 1, 3 )

-}
fromComponents : ( Float, Float, Float ) -> Vector3d
fromComponents =
    Types.Vector3d


{-| Construct a vector from the first given point to the second.

    startPoint =
        Point3d.fromCoordinates ( 1, 1, 1 )

    endPoint =
        Point3d.fromCoordinates ( 4, 5, 6 )

    Vector3d.from startPoint endPoint
    --> Vector3d.fromComponents ( 3, 4, 5 )

-}
from : Point3d -> Point3d -> Vector3d
from firstPoint secondPoint =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates firstPoint

        ( x2, y2, z2 ) =
            Point3d.coordinates secondPoint
    in
    fromComponents ( x2 - x1, y2 - y1, z2 - z1 )


{-| Construct a vector with the given length in the given direction.

    Vector3d.withLength 5 Direction3d.y
    --> Vector3d.fromComponents ( 0, 5, 0 )

-}
withLength : Float -> Direction3d -> Vector3d
withLength length_ direction_ =
    let
        ( dx, dy, dz ) =
            Direction3d.components direction_
    in
    fromComponents ( length_ * dx, length_ * dy, length_ * dz )


{-| Construct a 3D vector lying _on_ a sketch plane by providing a 2D vector
specified in XY coordinates _within_ the sketch plane.

    vector2d =
        Vector2d.fromComponents ( 2, 3 )

    Vector3d.on SketchPlane3d.xy vector2d
    --> Vector3d.fromComponents ( 2, 3, 0 )

    Vector3d.on SketchPlane3d.yz vector2d
    --> Vector3d.fromComponents ( 0, 2, 3 )

    Vector3d.on SketchPlane3d.zx vector2d
    --> Vector3d.fromComponents ( 3, 0, 2 )

A slightly more complex example:

    tiltedSketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.rotateAround Axis3d.x
                (degrees 45)

    Vector3d.on tiltedSketchPlane <|
        Vector2d.fromComponents ( 1, 1 )
    --> Vector3d.fromComponents ( 1, 0.7071, 0.7071 )

-}
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


{-| Construct an arbitrary vector perpendicular to the given vector. The exact
length and direction of the resulting vector are not specified, but it is
guaranteed to be perpendicular to the given vector and non-zero (unless the
given vector is itself zero).

    Vector3d.perpendicularTo
        (Vector3d.fromComponents ( 3, 0, 0 ))
    --> Vector3d.fromComponents ( 0, 0, -3 )

    Vector3d.perpendicularTo
        (Vector3d.fromComponents ( 1, 2, 3 ))
    --> Vector3d.fromComponents ( 0, -3, 2 )

    Vector3d.perpendicularTo Vector3d.zero
    --> Vector3d.zero

-}
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


{-| Construct a vector by interpolating from the first given vector to the
second, based on a parameter that ranges from zero to one.

    startVector =
        Vector3d.fromComponents ( 1, 2, 4 )

    endVector =
        Vector3d.fromComponents ( 1, 3, 8 )

    Vector3d.interpolateFrom startVector endVector 0.25
    --> Vector3d.fromComponents ( 1, 2.25, 5 )

Partial application may be useful:

    interpolatedVector : Float -> Vector3d
    interpolatedVector =
        Vector3d.interpolateFrom startVector endVector

    List.map interpolatedVector [ 0, 0.5, 1 ]
    --> [ Vector3d.fromComponents ( 1, 2, 4 )
    --> , Vector3d.fromComponents ( 1, 2, 6 )
    --> , Vector3d.fromComponents ( 1, 2, 8 )
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedVector -0.5
    --> Vector3d.fromComponents ( 1, 2, 2 )

    interpolatedVector 1.25
    --> Vector3d.fromComponents ( 1, 2, 9 )

-}
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


{-| Extract the components of a vector.

    Vector3d.fromComponents ( 2, 3, 4 )
        |> Vector3d.components
    --> ( 2, 3, 4 )

This combined with Elm's built-in tuple destructuring provides a convenient way
to extract the X, Y and Z components of a vector in one line of code:

    ( x, y, z ) =
        Vector3d.components vector

-}
components : Vector3d -> ( Float, Float, Float )
components (Types.Vector3d components_) =
    components_


{-| Get the X component of a vector.

    Vector3d.fromComponents ( 1, 2, 3 )
        |> Vector3d.xComponent
    --> 1

-}
xComponent : Vector3d -> Float
xComponent (Types.Vector3d ( x, _, _ )) =
    x


{-| Get the Y component of a vector.

    Vector3d.fromComponents ( 1, 2, 3 )
        |> Vector3d.yComponent
    --> 2

-}
yComponent : Vector3d -> Float
yComponent (Types.Vector3d ( _, y, _ )) =
    y


{-| Get the Z component of a vector.

    Vector3d.fromComponents ( 1, 2, 3 )
        |> Vector3d.zComponent
    --> 3

-}
zComponent : Vector3d -> Float
zComponent (Types.Vector3d ( _, _, z )) =
    z


{-| Find the component of a vector in an arbitrary direction, for example

    verticalSpeed =
        Vector3d.componentIn upDirection velocity

This is more general and flexible than using `xComponent`, `yComponent` or
`zComponent`, all of which can be expressed in terms of `componentIn`; for
example,

    Vector3d.zComponent vector

is equivalent to

    Vector3d.componentIn Direction3d.z vector

-}
componentIn : Direction3d -> Vector3d -> Float
componentIn direction_ vector =
    let
        ( dx, dy, dz ) =
            Direction3d.components direction_

        ( vx, vy, vz ) =
            components vector
    in
    vx * dx + vy * dy + vz * dz


{-| Compare two vectors within a tolerance. Returns true if the difference
between the two given vectors has magnitude less than the given tolerance.

    firstVector =
        Vector3d.fromComponents ( 2, 1, 3 )

    secondVector =
        Vector3d.fromComponents ( 2.0002, 0.9999, 3.0001 )

    Vector3d.equalWithin 1e-3 firstVector secondVector
    --> True

    Vector3d.equalWithin 1e-6 firstVector secondVector
    --> False

-}
equalWithin : Float -> Vector3d -> Vector3d -> Bool
equalWithin tolerance firstVector secondVector =
    squaredLength (difference firstVector secondVector) <= tolerance * tolerance


{-| Get the length (magnitude) of a vector.

    Vector3d.length (Vector3d.fromComponents ( 2, 1, 2 ))
    --> 3

-}
length : Vector3d -> Float
length vector =
    sqrt (squaredLength vector)


{-| Get the squared length of a vector. `squaredLength` is slightly faster than
`length`, so for example

    Vector3d.squaredLength vector > tolerance * tolerance

is equivalent to but slightly more efficient than

    Vector3d.length vector > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `length` is much more
readable!

-}
squaredLength : Vector3d -> Float
squaredLength vector =
    let
        ( x, y, z ) =
            components vector
    in
    x * x + y * y + z * z


{-| Attempt to find the direction of a vector. In the case of a zero vector,
returns `Nothing`.

    Vector3d.fromComponents ( 3, 0, 3 )
        |> Vector3d.direction
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (degrees 0)
    -->         (degrees 45)
    -->     )

    Vector3d.direction Vector3d.zero
    --> Nothing

-}
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


{-| Attempt to find the length and direction of a vector. In the case of a zero
vector, returns `Nothing`.

    vector =
        Vector3d.fromComponents ( 3, 0, 3 )

    Vector3d.lengthAndDirection vector
    --> Just
    -->     ( 4.2426
    -->     , Direction3d.fromAzimuthAndElevation
    -->         (degrees 0)
    -->         (degrees 45)
    -->     )

    Vector3d.lengthAndDirection Vector3d.zero
    --> Nothing

-}
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


{-| Normalize a vector to have a length of one. Zero vectors are left as-is.

    vector =
        Vector3d.fromComponents ( 3, 0, 4 )

    Vector3d.normalize vector
    --> Vector3d.fromComponents ( 0.6, 0, 0.8 )

    Vector3d.normalize Vector3d.zero
    --> Vector3d.zero

**Warning**: `Vector3d.direction` is safer since it forces you to explicitly
consider the case where the given vector is zero. `Vector3d.normalize` is
primarily useful for cases like generating WebGL meshes, where defaulting to a
zero vector for degenerate cases is acceptable, and the overhead of something
like

    Vector3d.direction vector
        |> Maybe.map Direction3d.toVector
        |> Maybe.withDefault Vector3d.zero

(which is functionally equivalent to `Vector3d.normalize vector`) is too high.

-}
normalize : Vector3d -> Vector3d
normalize vector =
    if vector == zero then
        zero
    else
        scaleBy (1 / length vector) vector


{-| Find the sum of two vectors.

    firstVector =
        Vector3d.fromComponents ( 1, 2, 3 )

    secondVector =
        Vector3d.fromComponents ( 4, 5, 6 )

    Vector3d.sum firstVector secondVector
    --> Vector3d.fromComponents ( 5, 7, 9 )

-}
sum : Vector3d -> Vector3d -> Vector3d
sum firstVector secondVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    fromComponents ( x1 + x2, y1 + y2, z1 + z2 )


{-| Find the difference between two vectors (the first vector minus the second).

    firstVector =
        Vector3d.fromComponents ( 5, 6, 7 )

    secondVector =
        Vector3d.fromComponents ( 1, 1, 1 )

    Vector3d.difference firstVector secondVector
    --> Vector3d.fromComponents ( 4, 5, 6 )

-}
difference : Vector3d -> Vector3d -> Vector3d
difference firstVector secondVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    fromComponents ( x1 - x2, y1 - y2, z1 - z2 )


{-| Find the dot product of two vectors.

    firstVector =
        Vector3d.fromComponents ( 1, 0, 2 )

    secondVector =
        Vector3d.fromComponents ( 3, 4, 5 )

    Vector3d.dotProduct firstVector secondVector
    --> 13

-}
dotProduct : Vector3d -> Vector3d -> Float
dotProduct firstVector secondVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    x1 * x2 + y1 * y2 + z1 * z2


{-| Find the cross product of two vectors.

    firstVector =
        Vector3d.fromComponents ( 2, 0, 0 )

    secondVector =
        Vector3d.fromComponents ( 0, 3, 0 )

    Vector3d.crossProduct firstVector secondVector
    --> Vector3d.fromComponents ( 0, 0, 6 )

-}
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


{-| Reverse the direction of a vector, negating its components.

    Vector3d.reverse (Vector3d.fromComponents ( 1, -3, 2 ))
    --> Vector3d.fromComponents ( -1, 3, -2 )

-}
reverse : Vector3d -> Vector3d
reverse vector =
    let
        ( x, y, z ) =
            components vector
    in
    fromComponents ( -x, -y, -z )


{-| Scale the length of a vector by a given scale.

    Vector3d.fromComponents ( 1, 2, 3 )
        |> Vector3d.scaleBy 3
    --> Vector3d.fromComponents ( 3, 6, 9 )

-}
scaleBy : Float -> Vector3d -> Vector3d
scaleBy scale vector =
    let
        ( x, y, z ) =
            components vector
    in
    fromComponents ( x * scale, y * scale, z * scale )


{-| Rotate a vector around a given axis by a given angle (in radians).

    vector =
        Vector3d.fromComponents ( 2, 0, 1 )

    Vector3d.rotateAround Axis3d.x (degrees 90) vector
    --> Vector3d.fromComponents ( 2, -1, 0 )

    Vector3d.rotateAround Axis3d.z (degrees 45) vector
    --> Vector3d.fromComponents ( 1.4142, 1.4142, 1 )

-}
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


{-| Mirror a vector across a plane.

    vector =
        Vector3d.fromComponents ( 1, 2, 3 )

    Vector3d.mirrorAcross Plane3d.xy vector
    --> Vector3d.fromComponents ( 1, 2, -3 )

    Vector3d.mirrorAcross Plane3d.yz vector
    --> Vector3d.fromComponents ( -1, 2, 3 )

-}
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


{-| Find the projection of a vector in a particular direction. Conceptually,
this means splitting the original vector into a portion parallel to the given
direction and a portion perpendicular to it, then returning the parallel
portion.

    vector =
        Vector3d.fromComponents ( 1, 2, 3 )

    Vector3d.projectionIn Direction3d.x vector
    --> Vector3d.fromComponents ( 1, 0, 0 )

    Vector3d.projectionIn Direction3d.z vector
    --> Vector3d.fromComponents ( 0, 0, 3 )

-}
projectionIn : Direction3d -> Vector3d -> Vector3d
projectionIn direction_ vector =
    direction_ |> withLength (vector |> componentIn direction_)


{-| Project a vector [orthographically](https://en.wikipedia.org/wiki/Orthographic_projection)
onto a plane. Conceptually, this means splitting the original vector into a
portion parallel to the plane (perpendicular to the plane's normal direction)
and a portion perpendicular to it (parallel to its normal direction), then
returning the parallel (in-plane) portion.

    vector =
        Vector3d.fromComponents ( 2, 1, 3 )

    Vector3d.projectOnto Plane3d.xy vector
    --> Vector3d.fromComponents ( 2, 1, 0 )

    Vector3d.projectOnto Plane3d.xz vector
    --> Vector3d.fromComponents ( 2, 0, 3 )

-}
projectOnto : Plane3d -> Vector3d -> Vector3d
projectOnto plane vector =
    difference vector (projectionIn (Plane3d.normalDirection plane) vector)


{-| Take a vector defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    vector =
        Vector3d.fromComponents ( 2, 0, 3 )

    Vector3d.relativeTo rotatedFrame vector
    --> Vector3d.fromComponents ( 1.732, -1, 3 )

-}
relativeTo : Frame3d -> Vector3d -> Vector3d
relativeTo frame vector =
    fromComponents
        ( componentIn (Frame3d.xDirection frame) vector
        , componentIn (Frame3d.yDirection frame) vector
        , componentIn (Frame3d.zDirection frame) vector
        )


{-| Take a vector defined in local coordinates relative to a given reference
frame, and return that vector expressed in global coordinates.

    vector =
        Vector3d.fromComponents ( 2, 0, 3 )

    Vector3d.placeIn rotatedFrame vector
    --> Vector3d.fromComponents ( 1.732, 1, 3 )

-}
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


{-| Project a vector into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the vector onto the plane and then expresses the projected vector in 2D
sketch coordinates.

    vector =
        Vector3d.fromComponents ( 2, 1, 3 )

    Vector3d.projectInto SketchPlane3d.xy vector
    --> Vector2d.fromComponents ( 2, 1 )

    Vector3d.projectInto SketchPlane3d.yz vector
    --> Vector2d.fromComponents ( 1, 3 )

    Vector3d.projectInto SketchPlane3d.zx vector
    --> Vector2d.fromComponents ( 3, 2 )

-}
projectInto : SketchPlane3d -> Vector3d -> Vector2d
projectInto sketchPlane vector =
    Vector2d.fromComponents
        ( componentIn (SketchPlane3d.xDirection sketchPlane) vector
        , componentIn (SketchPlane3d.yDirection sketchPlane) vector
        )
