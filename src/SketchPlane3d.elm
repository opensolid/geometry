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


module SketchPlane3d
    exposing
        ( SketchPlane3d
        , fromPlane
        , mirrorAcross
        , moveTo
        , normalAxis
        , normalDirection
        , offsetBy
        , on
        , originPoint
        , placeIn
        , relativeTo
        , reverseX
        , reverseY
        , rotateAround
        , rotateAroundOwn
        , throughPoints
        , toPlane
        , translateAlongOwn
        , translateBy
        , translateIn
        , unsafe
        , withNormalDirection
        , xAxis
        , xDirection
        , xy
        , xz
        , yAxis
        , yDirection
        , yx
        , yz
        , zx
        , zy
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/SketchPlane3d/icon.svg" alt="SketchPlane3d" width="160">

A `SketchPlane3d` represents a 2D planar coordinate system in 3D space, and is
defined by its origin point and X and Y directions (which are always
perpendicular to each other). Sketch planes are the primary tool for converting
back and forth between 2D and 3D coordinates:

  - 3D geometry such as points, directions and line segments can be projected
    _into_ a sketch plane, which effectively projects the geometry _onto_ the
    sketch plane and then expresses the projected geometry _in_ 2D coordinates.
  - 2D geometry can be place _onto_ a sketch plane to result in 3D geometry. For
    example, a 2D point placed onto a sketch plane will result in a 3D point
    _on_ that sketch plane that has the given 2D coordinate _in_ the sketch
    plane.

This allows you to create algorithms that project from 3D into 2D, perform some
calculations in 2D, then convert the result back to 3D.

Many 3D data types have `projectInto` functions that return the corresponding 2D
data type, and `on` functions for converting back to 3D. For example,
[`Triangle3d.projectInto`](Triangle3d#projectInto) returns a `Triangle2d` and
[`Triangle3d.on`](Triangle3d#on) returns a `Triangle3d`. These pairs of
functions are almost, but not quite, inverses of each other:

    triangle2d
        |> Triangle3d.on sketchPlane
        |> Triangle3d.projectInto sketchPlane

will just return the original `triangle2d` value (within roundoff error), while

    triangle3d
        |> Triangle3d.projectInto sketchPlane
        |> Triangle3d.on sketchPlane

is equivalent to

    triangle3d
        |> Triangle3d.projectOnto
            (SketchPlane3d.toPlane sketchPlane)

@docs SketchPlane3d


# Constants

These predefined sketch planes all have the global origin point as their origin
point, and use the two indicated global axes as their X and Y axes. For example,

    SketchPlane3d.originPoint SketchPlane3d.yz
    --> Point3d.origin

    SketchPlane3d.xDirection SketchPlane3d.yz
    --> Direction3d.y

    SketchPlane3d.yDirection SketchPlane3d.yz
    --> Direction3d.z

@docs xy, yx, yz, zy, zx, xz


# Constructors

Sketch planes can also be constructed from `Frame3d` values using
`Frame3d.xySketchPlane` etc.

@docs withNormalDirection, on, throughPoints, fromPlane, unsafe


# Conversions

@docs toPlane


# Properties

@docs originPoint, xDirection, yDirection, normalDirection, xAxis, yAxis, normalAxis


# Transformations

@docs offsetBy, reverseX, reverseY, moveTo, rotateAround, rotateAroundOwn, translateBy, translateIn, translateAlongOwn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types exposing (Frame3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias SketchPlane3d =
    Types.SketchPlane3d


{-| A sketch plane formed from the global X and Y axes.
-}
xy : SketchPlane3d
xy =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        }


{-| A sketch plane formed from the global Y and X axes.
-}
yx : SketchPlane3d
yx =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.x
        }


{-| A sketch plane formed from the global Y and Z axes.
-}
yz : SketchPlane3d
yz =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.z
        }


{-| A sketch plane formed from the global Z and Y axes.
-}
zy : SketchPlane3d
zy =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.y
        }


{-| A sketch plane formed from the global Z and X axes.
-}
zx : SketchPlane3d
zx =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.x
        }


{-| A sketch plane formed from the global X and Z axes.
-}
xz : SketchPlane3d
xz =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.z
        }


{-| Construct a sketch plane with the given normal direction, having the given
origin point. The X and Y basis directions of the sketch plane will:

  - be perpendicular to each other,
  - both be perpendicular to the given normal direction, and
  - have a cross product equal to the given normal direction.

This is useful when constructing 'scratch' sketch planes where the specific X/Y
directions are unimportant.

    sketchPlane =
        SketchPlane3d.withNormalDirection
            (Direction3d.fromAzimuthAndElevation
                (degrees 0)
                (degrees 60)
            )
            Point3d.origin

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.origin

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 0)
    -->     (degrees -30)

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.y

-}
withNormalDirection : Direction3d -> Point3d -> SketchPlane3d
withNormalDirection normalDirection_ originPoint_ =
    let
        ( xDirection_, yDirection_ ) =
            Direction3d.perpendicularBasis normalDirection_
    in
    unsafe
        { originPoint = originPoint_
        , xDirection = xDirection_
        , yDirection = yDirection_
        }


{-| Construct one sketch plane lying on another sketch plane, but with a
different origin point and X/Y directions. To do this, a `Frame2d` must be
provided that specifies the origin point and X/Y directions of the new sketch
plane, in 2D coordinates within the existing sketch plane. Whew!

    frame2d =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))
            |> Frame2d.rotateBy (degrees -30)

    sketchPlane =
        SketchPlane3d.on SketchPlane3d.yz frame2d

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.fromCoordinates ( 0, 2, 3 )

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 90)
    -->     (degrees -30)

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 90)
    -->     (degrees 60)

-}
on : SketchPlane3d -> Frame2d -> SketchPlane3d
on sketchPlane frame =
    unsafe
        { originPoint = Point3d.on sketchPlane (Frame2d.originPoint frame)
        , xDirection = Direction3d.on sketchPlane (Frame2d.xDirection frame)
        , yDirection = Direction3d.on sketchPlane (Frame2d.yDirection frame)
        }


{-| Construct a SketchPlane3d from the given plane;

    SketchPlane3d.fromPlane plane

is equivalent to

    SketchPlane3d.withNormalDirection
        (Plane3d.normalDirection plane)
        (Plane3d.originPoint plane)

Note that because the X and Y directions of the resulting sketch plane are
chosen arbitrarily, conversions may not work exactly as you expect. For example,
in the current implementation,

    sketchPlane =
        SketchPlane3d.fromPlane Plane3d.xy

is not equal to `SketchPlane3d.xy` (although the two sketch planes have the same
origin point and are coplanar):

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.origin

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.negativeY

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.positiveX

-}
fromPlane : Plane3d -> SketchPlane3d
fromPlane plane =
    withNormalDirection (Plane3d.normalDirection plane)
        (Plane3d.originPoint plane)


{-| Construct a sketch plane directly from its origin point and X and Y
directions:

    sketchPlane =
        SketchPlane3d.unsafe
            { originPoint =
                Point3d.fromCoordinates ( 2, 1, 3 )
            , xDirection = Direction3d.positiveY
            , yDirection = Direction3d.negativeZ
            }

If you construct a `SketchPlane3d` this way, **you must ensure that the X and Y
basis directions are perpendicular to each other**.

-}
unsafe : { originPoint : Point3d, xDirection : Direction3d, yDirection : Direction3d } -> SketchPlane3d
unsafe =
    Types.SketchPlane3d


{-| Attempt to construct a sketch plane that passes through the three given
points. Returns a sketch plane where:

  - The origin point is the first given point
  - The X direction is equal to the direction from the first given point to the
    second
  - The Y direction is chosen such that the third given point lies on the sketch
    plane and has a positive Y coordinate within the sketch plane (that is, it
    is on the positive Y side of the sketch plane's X axis)

If the three given points are collinear, returns `Nothing`.

    SketchPlane3d.throughPoints
        (Point3d.fromCoordinates ( 2, 0, 0 ))
        (Point3d.fromCoordinates ( 3, 0, 0 ))
        (Point3d.fromCoordinates ( 4, 1, 1 ))
    --> Just sketchPlane

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.fromCoordinates ( 2, 0, 0 )

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.x

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 90)
    -->     (degrees 45)

    SketchPlane3d.throughPoints
        (Point3d.fromCoordinates ( 2, 0, 0 ))
        (Point3d.fromCoordinates ( 3, 0, 0 ))
        (Point3d.fromCoordinates ( 4, 0, 0 ))
    --> Nothing

-}
throughPoints : Point3d -> Point3d -> Point3d -> Maybe SketchPlane3d
throughPoints firstPoint secondPoint thirdPoint =
    Direction3d.from firstPoint secondPoint
        |> Maybe.andThen
            (\xDirection_ ->
                let
                    firstCandidate =
                        Vector3d.from firstPoint thirdPoint

                    secondCandidate =
                        Vector3d.from secondPoint thirdPoint

                    firstSquaredLength =
                        Vector3d.squaredLength firstCandidate

                    secondSquaredLength =
                        Vector3d.squaredLength secondCandidate

                    chosenVector =
                        if firstSquaredLength <= secondSquaredLength then
                            firstCandidate
                        else
                            secondCandidate

                    xDirectionVector =
                        Direction3d.toVector xDirection_

                    normalVector =
                        Vector3d.crossProduct xDirectionVector chosenVector

                    yVector =
                        Vector3d.crossProduct normalVector xDirectionVector
                in
                Vector3d.direction yVector
                    |> Maybe.map
                        (\yDirection_ ->
                            unsafe
                                { originPoint = firstPoint
                                , xDirection = xDirection_
                                , yDirection = yDirection_
                                }
                        )
            )


{-| Get the origin point of a sketch plane.

    SketchPlane3d.originPoint SketchPlane3d.xy
    --> Point3d.origin

-}
originPoint : SketchPlane3d -> Point3d
originPoint (Types.SketchPlane3d properties) =
    properties.originPoint


{-| Get the X direction of a sketch plane (the direction of the sketch plane's
X axis).

    SketchPlane3d.xDirection SketchPlane3d.zx
    --> Direction3d.z

-}
xDirection : SketchPlane3d -> Direction3d
xDirection (Types.SketchPlane3d properties) =
    properties.xDirection


{-| Get the Y direction of a sketch plane (the direction of the sketch plane's
Y axis).

    SketchPlane3d.yDirection SketchPlane3d.zx
    --> Direction3d.x

-}
yDirection : SketchPlane3d -> Direction3d
yDirection (Types.SketchPlane3d properties) =
    properties.yDirection


{-| Get the normal direction to a sketch plane. This is equal to the cross
product of the sketch plane's X and Y directions.

    SketchPlane3d.normalDirection SketchPlane3d.xy
    --> Direction3d.z

    SketchPlane3d.normalDirection SketchPlane3d.xz
    --> Direction3d.negativeY

-}
normalDirection : SketchPlane3d -> Direction3d
normalDirection sketchPlane =
    let
        normalVector =
            Vector3d.crossProduct
                (Direction3d.toVector (xDirection sketchPlane))
                (Direction3d.toVector (yDirection sketchPlane))
    in
    Direction3d.unsafe (Vector3d.components normalVector)


{-| Get the X axis of a sketch plane. A 2D X coordinate within the sketch plane
corresponds to a distance along this axis in 3D.

    SketchPlane3d.xAxis SketchPlane3d.zx
    --> Axis3d.z

-}
xAxis : SketchPlane3d -> Axis3d
xAxis (Types.SketchPlane3d sketchPlane) =
    Axis3d.through sketchPlane.originPoint sketchPlane.xDirection


{-| Get the Y axis of a sketch plane. A 2D Y coordinate within the sketch plane
corresponds to a distance along this axis in 3D.

    SketchPlane3d.yAxis SketchPlane3d.zx
    --> Axis3d.x

-}
yAxis : SketchPlane3d -> Axis3d
yAxis (Types.SketchPlane3d sketchPlane) =
    Axis3d.through sketchPlane.originPoint sketchPlane.yDirection


{-| Get the normal axis to a sketch plane (the axis formed from the sketch
plane's origin point and normal direction).

    SketchPlane3d.normalAxis SketchPlane3d.xy
    --> Axis3d.z

    SketchPlane3d.normalAxis SketchPlane3d.xz
    --> Axis3d.reverse Axis3d.y

-}
normalAxis : SketchPlane3d -> Axis3d
normalAxis sketchPlane =
    Axis3d.through (originPoint sketchPlane) (normalDirection sketchPlane)


{-| Convert a `SketchPlane3d` to a `Plane3d` with the same origin point and
normal direction.

    SketchPlane3d.toPlane SketchPlane3d.xy
    --> Plane3d.xy

    SketchPlane3d.toPlane SketchPlane3d.yx
    --> Plane3d.reverseNormal Plane3d.xy

-}
toPlane : SketchPlane3d -> Plane3d
toPlane sketchPlane =
    Plane3d.through (originPoint sketchPlane) (normalDirection sketchPlane)


{-| Shift a sketch plane in its own normal direction by the given (signed)
distance.

    SketchPlane3d.offsetBy -2.0 SketchPlane3d.xy
        |> SketchPlane3d.originPoint
    --> Point3d.fromCoordinates ( 0, 0, -2 )

    SketchPlane3d.offsetBy 1.0 SketchPlane3d.zx
        |> SketchPlane3d.originPoint
    --> Point3d.fromCoordinates ( 0, 1, 0 )

-}
offsetBy : Float -> SketchPlane3d -> SketchPlane3d
offsetBy distance sketchPlane =
    let
        displacement =
            Vector3d.withLength distance (normalDirection sketchPlane)
    in
    translateBy displacement sketchPlane


{-| Reverse the X direction of a sketch plane, leaving its Y direction and
origin point unchanged.

    sketchPlane =
        SketchPlane3d.reverseX SketchPlane3d.yz

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.origin

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.negativeY

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.z

-}
reverseX : SketchPlane3d -> SketchPlane3d
reverseX sketchPlane =
    unsafe
        { originPoint = originPoint sketchPlane
        , xDirection = Direction3d.reverse (xDirection sketchPlane)
        , yDirection = yDirection sketchPlane
        }


{-| Reverse the Y direction of a sketch plane, leaving its X direction and
origin point unchanged.

    sketchPlane =
        SketchPlane3d.reverseY SketchPlane3d.yz

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.origin

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.y

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.negativeZ

-}
reverseY : SketchPlane3d -> SketchPlane3d
reverseY sketchPlane =
    unsafe
        { originPoint = originPoint sketchPlane
        , xDirection = xDirection sketchPlane
        , yDirection = Direction3d.reverse (yDirection sketchPlane)
        }


{-| Set the origin point of the given sketch plane to the given point, leaving
its X and Y directions unchanged.

    newOrigin =
        Point3d.fromCoordinates ( 2, 1, 3 )

    sketchPlane =
        SketchPlane3d.moveTo newOrigin SketchPlane3d.yz

    SketchPlane3d.originPoint sketchPlane
    --> newOrigin

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.y

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.z

-}
moveTo : Point3d -> SketchPlane3d -> SketchPlane3d
moveTo newOrigin sketchPlane =
    unsafe
        { originPoint = newOrigin
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


{-| Rotate a sketch plane around an axis by a given angle (in radians). The
sketch plane's origin point and X and Y directions will all be rotated around
the given axis.

    SketchPlane3d.xy
        |> SketchPlane3d.rotateAround Axis3d.x (degrees 90)
    --> SketchPlane3d.xz

-}
rotateAround : Axis3d -> Float -> SketchPlane3d -> SketchPlane3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
    \sketchPlane ->
        unsafe
            { originPoint = rotatePoint (originPoint sketchPlane)
            , xDirection = rotateDirection (xDirection sketchPlane)
            , yDirection = rotateDirection (yDirection sketchPlane)
            }


{-| Rotate a sketch plane around one of its own axes by a given angle (in
radians).

The first argument is a function that returns the axis to rotate around, given
the current sketch plane. The majority of the time this will be either
`SketchPlane3d.xAxis` or `SketchPlane3d.yAxis`.

This function is convenient when constructing sketch planes via a series of
transformations. For example,

    sketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.translateBy
                (Vector3d.fromComponents ( 1, 0, 0 ))
            |> SketchPlane3d.rotateAroundOwn
                SketchPlane3d.yAxis
                (degrees -45)

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.fromCoordinates ( 1, 0, 0 )

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 0)
    -->     (degrees 45)

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.y

Note that since the rotation was around the sketch plane's own Y axis (which
passes through the sketch plane's origin point) instead of the global Y axis,
the origin point itself was not affected by the rotation.

-}
rotateAroundOwn : (SketchPlane3d -> Axis3d) -> Float -> SketchPlane3d -> SketchPlane3d
rotateAroundOwn axis angle sketchPlane =
    rotateAround (axis sketchPlane) angle sketchPlane


{-| Translate a sketch plane by a given displacement.

    displacement =
        Vector3d.fromComponents ( 2, 1, 3 )

    sketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.translateBy displacement

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.fromCoordinates ( 2, 1, 3 )

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.x

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.y

-}
translateBy : Vector3d -> SketchPlane3d -> SketchPlane3d
translateBy vector sketchPlane =
    unsafe
        { originPoint = Point3d.translateBy vector (originPoint sketchPlane)
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


{-| Translate a sketch plane in a given direction by a given distance;

    SketchPlane3d.translateIn direction distance

is equivalent to

    SketchPlane3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> SketchPlane3d -> SketchPlane3d
translateIn direction distance sketchPlane =
    translateBy (Vector3d.withLength distance direction) sketchPlane


{-| Translate a sketch plane along one of its own axes by a given distance.

The first argument is a function that returns the axis to translate along, given
the current sketch plane. The majority of the time this will be either
`SketchPlane3d.xAxis` or `SketchPlane3d.yAxis`.

This function is convenient when constructing frames via a series of
transformations. For example,

    sketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.rotateAround
                Axis3d.x
                (degrees 45)
            |> SketchPlane3d.translateAlongOwn
                SketchPlane3d.yAxis
                2

means 'take the global XY sketch plane, rotate it around the global X axis by
45 degrees, then translate the result 2 units along its own (rotated) Y axis',
resulting in

    SketchPlane3d.originPoint sketchPlane
    --> Point3d.fromCoordinates ( 0, 1.4142, 1.4142 )

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.x

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 90)
    -->     (degrees 45)

-}
translateAlongOwn : (SketchPlane3d -> Axis3d) -> Float -> SketchPlane3d -> SketchPlane3d
translateAlongOwn axis distance frame =
    let
        displacement =
            Vector3d.withLength distance (Axis3d.direction (axis frame))
    in
    translateBy displacement frame


{-| Mirror a sketch plane across a plane.

    sketchPlane =
        SketchPlane3d.yz
            |> SketchPlane3d.moveTo
                (Point2d.fromCoordinates ( 2, 1, 3 ))

    mirroredSketchPlane =
        SketchPlane3d.mirrorAcross Plane3d.xy sketchPlane

    SketchPlane3d.originPoint sketchPlane
    --> Point2d.fromCoordinates ( 2, 1, -3 )

    SketchPlane3d.xDirection sketchPlane
    --> Direction3d.y

    SketchPlane3d.yDirection sketchPlane
    --> Direction3d.negativeZ

-}
mirrorAcross : Plane3d -> SketchPlane3d -> SketchPlane3d
mirrorAcross plane =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane

        mirrorDirection =
            Direction3d.mirrorAcross plane
    in
    \sketchPlane ->
        unsafe
            { originPoint = mirrorPoint (originPoint sketchPlane)
            , xDirection = mirrorDirection (xDirection sketchPlane)
            , yDirection = mirrorDirection (yDirection sketchPlane)
            }


{-| Take a sketch plane defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.
-}
relativeTo : Frame3d -> SketchPlane3d -> SketchPlane3d
relativeTo frame =
    let
        relativePoint =
            Point3d.relativeTo frame

        relativeDirection =
            Direction3d.relativeTo frame
    in
    \sketchPlane ->
        unsafe
            { originPoint = relativePoint (originPoint sketchPlane)
            , xDirection = relativeDirection (xDirection sketchPlane)
            , yDirection = relativeDirection (yDirection sketchPlane)
            }


{-| Take a sketch plane defined in local coordinates relative to a given
reference frame, and return that sketch plane expressed in global coordinates.
-}
placeIn : Frame3d -> SketchPlane3d -> SketchPlane3d
placeIn frame =
    let
        placePoint =
            Point3d.placeIn frame

        placeDirection =
            Direction3d.placeIn frame
    in
    \sketchPlane ->
        unsafe
            { originPoint = placePoint (originPoint sketchPlane)
            , xDirection = placeDirection (xDirection sketchPlane)
            , yDirection = placeDirection (yDirection sketchPlane)
            }
