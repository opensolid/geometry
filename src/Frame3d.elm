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


module Frame3d
    exposing
        ( Frame3d
        , atCoordinates
        , atPoint
        , isRightHanded
        , mirrorAcross
        , moveTo
        , originPoint
        , placeIn
        , relativeTo
        , reverseX
        , reverseY
        , reverseZ
        , rotateAround
        , rotateAroundOwn
        , translateAlongOwn
        , translateBy
        , translateIn
        , unsafe
        , withXDirection
        , withYDirection
        , withZDirection
        , xAxis
        , xDirection
        , xyPlane
        , xySketchPlane
        , xyz
        , xzPlane
        , xzSketchPlane
        , yAxis
        , yDirection
        , yxPlane
        , yxSketchPlane
        , yzPlane
        , yzSketchPlane
        , zAxis
        , zDirection
        , zxPlane
        , zxSketchPlane
        , zyPlane
        , zySketchPlane
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Frame3d/icon.svg" alt="Frame3d" width="160">

A `Frame3d` has an origin point and a set of X, Y and Z directions (which are
always perpendicular to each other). It can be thought of as:

  - A local coordinate system: Most geometric types have associated `relativeTo`
    and `placeIn` functions that convert values of that type from global
    coordinates to local coordinates in a particular frame, and vice versa.
  - A set of axes and planes: It is often convenient to (for example) rotate
    around the Z axis of a frame, or mirror across its XY plane. Frames can
    also themselves be translated, rotated and mirrored!
  - A combined 3D position and orientation: For example, a `Frame3d` could be
    used to define the position and orientation of a spaceship in a 3D game.
    Movement of the ship would then be done by translating and rotating the
    frame.

@docs Frame3d


# Constants

@docs xyz


# Constructors

The `withXDirection`, `withYDirection` and `withZDirection` functions all
construct a new `Frame3d` with the given axis direction, having the given origin
point. The other two directions will be chosen arbitrarily. This can be useful
when constructing 'scratch' frames where (for example) you want a particular Z
direction but the specific X/Y directions are unimportant:

    zDirection =
        Direction3d.fromAzimuthAndElevation
            (degrees 0)
            (degrees 60)

    frame =
        Frame3d.withZDirection zDirection Point3d.origin

    Frame3d.zDirection frame
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 0)
    -->     (degrees 60)

    Frame3d.originPoint frame
    --> Point3d.origin

    Frame3d.xDirection frame
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 0)
    -->     (degrees -30)

    Frame3d.yDirection frame
    --> Direction3d.y

No guarantees are given about the other two directions other than that the three
directions will be mutually perpendicular, and will be oriented so that the
resulting frame is [right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness).

@docs withXDirection, withYDirection, withZDirection, atPoint, atCoordinates, unsafe


# Properties

@docs originPoint, xDirection, yDirection, zDirection, isRightHanded


## Axes

@docs xAxis, yAxis, zAxis


## Planes

The following functions all return planes with the same origin point as the
given frame, but with varying normal directions. In each case the normal
direction of the resulting plane is given by the cross product of the two
indicated basis directions (assuming a right-handed frame); for example,

    Frame3d.xyPlane Frame3d.xyz
    --> Plane3d.through Point3d.origin
    -->     Direction3d.positiveZ

since the cross product of the X and Y basis directions of a frame is equal to
its Z basis direction. And since reversing the order of arguments in a cross
product reverses the sign of the result,

    Frame3d.yxPlane Frame3d.xyz
    --> Plane3d.through Point3d.origin
    -->     Direction3d.negativeZ

@docs xyPlane, yxPlane, yzPlane, zyPlane, zxPlane, xzPlane


## Sketch planes

These functions all form a `SketchPlane3d` from two axes of the given frame. The
X and Y axes of the sketch plane will correspond to the two indicated axes. For
example,

    yzSketchPlane =
        Frame3d.yzSketchPlane Frame3d.xyz

    SketchPlane3d.originPoint yzSketchPlane
    --> Point3d.origin

    SketchPlane3d.xDirection yzSketchPlane
    --> Direction3d.y

    SketchPlane3d.yDirection yzSketchPlane
    --> Direction3d.z

Note that this can be confusing - for example, a local X coordinate in the above
sketch plane corresponds to a global Y coordinate, and a local Y coordinate
corresponds to a global Z coordinate!

@docs xySketchPlane, yxSketchPlane, yzSketchPlane, zySketchPlane, zxSketchPlane, xzSketchPlane


# Transformations

@docs reverseX, reverseY, reverseZ, moveTo, rotateAround, rotateAroundOwn, translateBy, translateIn, translateAlongOwn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Geometry.Types as Types
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Frame3d =
    Types.Frame3d


{-| The global XYZ frame.

    Frame3d.originPoint Frame3d.xyz
    --> Point3d.origin

    Frame3d.xDirection Frame3d.xyz
    --> Direction3d.x

    Frame3d.yDirection Frame3d.xyz
    --> Direction3d.y

    Frame3d.zDirection Frame3d.xyz
    --> Direction3d.z

-}
xyz : Frame3d
xyz =
    atPoint Point3d.origin


{-| Construct a frame with the given origin point and X direction.
-}
withXDirection : Direction3d -> Point3d -> Frame3d
withXDirection xDirection_ originPoint_ =
    let
        ( yDirection_, zDirection_ ) =
            Direction3d.perpendicularBasis xDirection_
    in
    unsafe
        { originPoint = originPoint_
        , xDirection = xDirection_
        , yDirection = yDirection_
        , zDirection = zDirection_
        }


{-| Construct a frame with the given origin point and Y direction.
-}
withYDirection : Direction3d -> Point3d -> Frame3d
withYDirection yDirection_ originPoint_ =
    let
        ( zDirection_, xDirection_ ) =
            Direction3d.perpendicularBasis yDirection_
    in
    unsafe
        { originPoint = originPoint_
        , xDirection = xDirection_
        , yDirection = yDirection_
        , zDirection = zDirection_
        }


{-| Construct a frame with the given origin point and Z direction.
-}
withZDirection : Direction3d -> Point3d -> Frame3d
withZDirection zDirection_ originPoint_ =
    let
        ( xDirection_, yDirection_ ) =
            Direction3d.perpendicularBasis zDirection_
    in
    unsafe
        { originPoint = originPoint_
        , xDirection = xDirection_
        , yDirection = yDirection_
        , zDirection = zDirection_
        }


{-| Construct a frame directly from its origin point and X, Y and Z directions:

    frame =
        Frame3d.unsafe
            { originPoint =
                Point3d.fromCoordinates ( 2, 1, 3 )
            , xDirection =
                Direction3d.unsafe ( 0.8, 0.6, 0 )
            , yDirection =
                Direction3d.unsafe ( -0.6, 0.8, 0 )
            , zDirection =
                Direction3d.unsafe ( 0, 0, 1 )
            }

In this case **you must be careful to ensure that the X, Y and Z directions are
perpendicular**. (You will likely also want to make sure that they form a
[right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
coordinate system.) To construct sets of mutually perpendicular directions,
[`Direction3d.orthonormalize`](Direction3d#orthonormalize),
[`Direction3d.orthogonalize`](Direction3d#orthogonalize), or
[`Direction3d.perpendicularBasis`](Direction3d#perpendicularBasis) may be
useful.

-}
unsafe : { originPoint : Point3d, xDirection : Direction3d, yDirection : Direction3d, zDirection : Direction3d } -> Frame3d
unsafe =
    Types.Frame3d


{-| Construct a frame aligned with the global XYZ frame but with the given
origin point.

    frame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 2, 1, 3 ))

    Frame3d.originPoint frame
    --> Point3d.fromCoordinates ( 2, 1, 3 )

    Frame3d.xDirection frame
    --> Direction3d.x

    Frame3d.yDirection frame
    --> Direction3d.y

    Frame3d.zDirection frame
    --> Direction3d.z

-}
atPoint : Point3d -> Frame3d
atPoint point =
    unsafe
        { originPoint = point
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        , zDirection = Direction3d.z
        }


{-| Shorthand for `Frame3d.atPoint`;

    Frame3d.atCoordinates ( x, y, z )

is equivalent to

    Frame3d.atPoint (Point3d.fromCoordinates ( x, y, z ))

-}
atCoordinates : ( Float, Float, Float ) -> Frame3d
atCoordinates coordinates =
    atPoint (Point3d.fromCoordinates coordinates)


{-| Get the origin point of a given frame.

    Frame3d.originPoint Frame3d.xyz
    --> Point3d.origin

-}
originPoint : Frame3d -> Point3d
originPoint (Types.Frame3d properties) =
    properties.originPoint


{-| Get the X direction of a given frame.

    Frame3d.xDirection Frame3d.xyz
    --> Direction3d.x

-}
xDirection : Frame3d -> Direction3d
xDirection (Types.Frame3d properties) =
    properties.xDirection


{-| Get the Y direction of a given frame.

    Frame3d.yDirection Frame3d.xyz
    --> Direction3d.y

-}
yDirection : Frame3d -> Direction3d
yDirection (Types.Frame3d properties) =
    properties.yDirection


{-| Get the Z direction of a given frame.

    Frame3d.zDirection Frame3d.xyz
    --> Direction3d.z

-}
zDirection : Frame3d -> Direction3d
zDirection (Types.Frame3d properties) =
    properties.zDirection


{-| Check if a frame is [right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness).

    Frame3d.isRightHanded Frame3d.xyz
    --> True

    Frame3d.isRightHanded (Frame3d.reverseZ Frame3d.xyz)
    --> False

All predefined frames are right-handed, and most operations on frames preserve
handedness, so about the only ways to end up with a left-handed frame are by
constructing one explicitly with `unsafe` or by mirroring a right-handed frame.

-}
isRightHanded : Frame3d -> Bool
isRightHanded frame =
    let
        xVector =
            Direction3d.toVector (xDirection frame)

        yVector =
            Direction3d.toVector (yDirection frame)

        zVector =
            Direction3d.toVector (zDirection frame)
    in
    Vector3d.dotProduct zVector (Vector3d.crossProduct xVector yVector) > 0


{-| Get the X axis of a given frame (the axis formed from the frame's origin
point and X direction).

    Frame3d.xAxis Frame3d.xyz
    --> Axis3d.x

-}
xAxis : Frame3d -> Axis3d
xAxis (Types.Frame3d frame) =
    Axis3d.through frame.originPoint frame.xDirection


{-| Get the Y axis of a given frame (the axis formed from the frame's origin
point and Y direction).

    Frame3d.yAxis Frame3d.xyz
    --> Axis3d.y

-}
yAxis : Frame3d -> Axis3d
yAxis (Types.Frame3d frame) =
    Axis3d.through frame.originPoint frame.yDirection


{-| Get the Z axis of a given frame (the axis formed from the frame's origin
point and Z direction).

    Frame3d.zAxis Frame3d.xyz
    --> Axis3d.z

-}
zAxis : Frame3d -> Axis3d
zAxis (Types.Frame3d frame) =
    Axis3d.through frame.originPoint frame.zDirection


{-| Get a plane with normal direction equal to the frame's positive Z direction.
-}
xyPlane : Frame3d -> Plane3d
xyPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint frame.zDirection


{-| Get a plane with normal direction equal to the frame's negative Z direction.
-}
yxPlane : Frame3d -> Plane3d
yxPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint (Direction3d.reverse frame.zDirection)


{-| Get a plane with normal direction equal to the frame's positive X direction.
-}
yzPlane : Frame3d -> Plane3d
yzPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint frame.xDirection


{-| Get a plane with normal direction equal to the frame's negative X direction.
-}
zyPlane : Frame3d -> Plane3d
zyPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint (Direction3d.reverse frame.xDirection)


{-| Get a plane with normal direction equal to the frame's positive Y direction.
-}
zxPlane : Frame3d -> Plane3d
zxPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint frame.yDirection


{-| Get a plane with normal direction equal to the frame's negative Y direction.
-}
xzPlane : Frame3d -> Plane3d
xzPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint (Direction3d.reverse frame.yDirection)


{-| Form a sketch plane from the given frame's X and Y axes.
-}
xySketchPlane : Frame3d -> SketchPlane3d
xySketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


{-| Form a sketch plane from the given frame's Y and X axes.
-}
yxSketchPlane : Frame3d -> SketchPlane3d
yxSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = yDirection frame
        , yDirection = xDirection frame
        }


{-| Form a sketch plane from the given frame's Y and Z axes.
-}
yzSketchPlane : Frame3d -> SketchPlane3d
yzSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = yDirection frame
        , yDirection = zDirection frame
        }


{-| Form a sketch plane from the given frame's Z and Y axes.
-}
zySketchPlane : Frame3d -> SketchPlane3d
zySketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = zDirection frame
        , yDirection = yDirection frame
        }


{-| Form a sketch plane from the given frame's Z and X axes.
-}
zxSketchPlane : Frame3d -> SketchPlane3d
zxSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = zDirection frame
        , yDirection = xDirection frame
        }


{-| Form a sketch plane from the given frame's X and Z axes.
-}
xzSketchPlane : Frame3d -> SketchPlane3d
xzSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = zDirection frame
        }


{-| Reverse the X direction of a frame.

    Frame3d.xDirection (Frame3d.reverseX Frame3d.xyz)
    --> Direction3d.negativeX

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
reverseX : Frame3d -> Frame3d
reverseX frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = Direction3d.reverse (xDirection frame)
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| Reverse the Y direction of a frame.

    Frame3d.yDirection (Frame3d.reverseY Frame3d.xyz)
    --> Direction3d.negativeY

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
reverseY : Frame3d -> Frame3d
reverseY frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = Direction3d.reverse (yDirection frame)
        , zDirection = zDirection frame
        }


{-| Reverse the Z direction of a frame.

    Frame3d.zDirection (Frame3d.reverseZ Frame3d.xyz)
    --> Direction3d.negativeZ

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
reverseZ : Frame3d -> Frame3d
reverseZ frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = Direction3d.reverse (zDirection frame)
        }


{-| Move a frame so that it has the given origin point but unchanged
orientation.

    point =
        Point3d.fromCoordinates ( 2, 1, 3 )

    Frame3d.moveTo point Frame3d.xyz
    --> Frame3d.atPoint point

-}
moveTo : Point3d -> Frame3d -> Frame3d
moveTo newOrigin frame =
    unsafe
        { originPoint = newOrigin
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| Rotate a frame around an axis by a given angle (in radians). The frame's
origin point and basis directions will all be rotated around the given axis.

    frame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 2, 1, 3 ))

    rotatedFrame =
        Frame3d.rotateAround Axis3d.z (degrees 90) frame

    Frame3d.originPoint rotatedFrame
    --> Point3d.fromCoordinates ( -1, 2, 3 )

    Frame3d.xDirection rotatedFrame
    --> Direction3d.y

    Frame3d.yDirection rotatedFrame
    --> Direction3d.negativeX

    Frame3d.zDirection rotatedFrame
    --> Direction3d.z

-}
rotateAround : Axis3d -> Float -> Frame3d -> Frame3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
    \frame ->
        unsafe
            { originPoint = rotatePoint (originPoint frame)
            , xDirection = rotateDirection (xDirection frame)
            , yDirection = rotateDirection (yDirection frame)
            , zDirection = rotateDirection (zDirection frame)
            }


{-| Rotate a frame around one of its own axes by a given angle (in radians).

The first argument is a function that returns the axis to rotate around, given
the current frame. The majority of the time this will be either `Frame3d.xAxis`,
`Frame3d.yAxis` or `Frame3d.zAxis`. Compare the following to the above example
for `rotateAround`:

    frame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 2, 1, 3 ))

    rotatedFrame =
        frame
            |> Frame3d.rotateAroundOwn Frame3d.zAxis
                (degrees 90)

    Frame3d.originPoint rotatedFrame
    --> Point3d.fromCoordinates ( 2, 1, 3 )

    Frame3d.xDirection rotatedFrame
    --> Direction3d.y

    Frame3d.yDirection rotatedFrame
    --> Direction3d.negativeX

    Frame3d.zDirection rotatedFrame
    --> Direction3d.z

Since the rotation is done around the frame's own Z axis (which passes through
the frame's origin point), the origin point remains the same after rotation.

In this example the frame's Z axis has the same orientation as the global Z axis
so the frame's basis directions are rotated the same way, but in more complex
examples involving rotated frames a rotation around (for example) the frame's
own Z axis may be completely different from a rotation around the global Z axis.

-}
rotateAroundOwn : (Frame3d -> Axis3d) -> Float -> Frame3d -> Frame3d
rotateAroundOwn axis angle frame =
    rotateAround (axis frame) angle frame


{-| Translate a frame by a given displacement.

    frame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 2, 1, 3 ))

    displacement =
        Vector3d.fromComponents ( 1, 1, 1 )

    Frame3d.translateBy displacement frame
    --> Frame3d.atPoint
    -->     (Point3d.fromCoordinates ( 3, 2, 4 ))

-}
translateBy : Vector3d -> Frame3d -> Frame3d
translateBy vector frame =
    unsafe
        { originPoint = Point3d.translateBy vector (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| Translate a frame in a given direction by a given distance;

    Frame3d.translateIn direction distance

is equivalent to

    Frame3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> Frame3d -> Frame3d
translateIn direction distance frame =
    translateBy (Vector3d.withLength distance direction) frame


{-| Translate a frame along one of its own axes by a given distance.

The first argument is a function that returns the axis to translate along, given
the current frame. The majority of the time this will be either `Frame3d.xAxis`,
`Frame3d.yAxis` or `Frame3d.zAxis`.

This function is convenient when constructing frames via a series of
transformations. For example,

    point =
        Point3d.fromCoordinates ( 2, 0, 0 )

    frame =
        Frame3d.atPoint point
            |> Frame3d.rotateAroundOwn Frame3d.zAxis
                (degrees 45)
            |> Frame3d.translateAlongOwn Frame3d.xAxis 2

means "construct a frame at the point (2, 0, 0), rotate it around its own Z axis
counterclockwise by 45 degrees, then translate it along its own (rotated) X axis
by 2 units", resulting in

    Frame3d.originPoint frame
    --> Point3d.fromCoordinates ( 3.4142, 1.4142, 0 )

    Frame3d.xDirection frame
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 45)
    -->     (degrees 0)

    Frame3d.yDirection frame
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 135)
    -->     (degrees 0)

    Frame3d.zDirection frame
    --> Direction3d.z

-}
translateAlongOwn : (Frame3d -> Axis3d) -> Float -> Frame3d -> Frame3d
translateAlongOwn axis distance frame =
    let
        displacement =
            Vector3d.withLength distance (Axis3d.direction (axis frame))
    in
    translateBy displacement frame


{-| Mirror a frame across a plane.

    frame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 2, 1, 3 ))

    mirroredFrame =
        Frame3d.mirrorAcross Plane3d.xy frame

    Frame3d.originPoint mirroredFrame
    --> Point3d.fromCoordinates ( 2, 1, -3 )

    Frame3d.xDirection mirroredFrame
    --> Direction3d.x

    Frame3d.yDirection mirroredFrame
    --> Direction3d.y

    Frame3d.zDirection mirroredFrame
    --> Direction3d.negativeZ

Note that this will switch the [handedness](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
of the frame.

-}
mirrorAcross : Plane3d -> Frame3d -> Frame3d
mirrorAcross plane =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane

        mirrorDirection =
            Direction3d.mirrorAcross plane
    in
    \frame ->
        unsafe
            { originPoint = mirrorPoint (originPoint frame)
            , xDirection = mirrorDirection (xDirection frame)
            , yDirection = mirrorDirection (yDirection frame)
            , zDirection = mirrorDirection (zDirection frame)
            }


{-| Take two frames defined in global coordinates, and return the second one
expressed in local coordinates relative to the first.
-}
relativeTo : Frame3d -> Frame3d -> Frame3d
relativeTo otherFrame =
    let
        relativePoint =
            Point3d.relativeTo otherFrame

        relativeDirection =
            Direction3d.relativeTo otherFrame
    in
    \frame ->
        unsafe
            { originPoint = relativePoint (originPoint frame)
            , xDirection = relativeDirection (xDirection frame)
            , yDirection = relativeDirection (yDirection frame)
            , zDirection = relativeDirection (zDirection frame)
            }


{-| Take one frame defined in global coordinates and a second frame defined
in local coordinates relative to the first frame, and return the second frame
expressed in global coordinates.
-}
placeIn : Frame3d -> Frame3d -> Frame3d
placeIn otherFrame =
    let
        placePoint =
            Point3d.placeIn otherFrame

        placeDirection =
            Direction3d.placeIn otherFrame
    in
    \frame ->
        unsafe
            { originPoint = placePoint (originPoint frame)
            , xDirection = placeDirection (xDirection frame)
            , yDirection = placeDirection (yDirection frame)
            , zDirection = placeDirection (zDirection frame)
            }
