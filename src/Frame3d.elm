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
direction but the specific X/Y directions are unimportant.

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
indicated basis directions (assuming a right-handed frame).

@docs xyPlane, yxPlane, yzPlane, zyPlane, zxPlane, xzPlane


## Sketch planes

These functions all form a `SketchPlane3d` from two axes of the given frame. The
X and Y axes of the sketch plane will correspond to the two indicated axes.

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


{-| -}
xyz : Frame3d
xyz =
    atPoint Point3d.origin


{-| -}
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


{-| -}
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


{-| -}
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


{-| -}
unsafe : { originPoint : Point3d, xDirection : Direction3d, yDirection : Direction3d, zDirection : Direction3d } -> Frame3d
unsafe =
    Types.Frame3d


{-| -}
atPoint : Point3d -> Frame3d
atPoint point =
    unsafe
        { originPoint = point
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        , zDirection = Direction3d.z
        }


{-| -}
atCoordinates : ( Float, Float, Float ) -> Frame3d
atCoordinates coordinates =
    atPoint (Point3d.fromCoordinates coordinates)


{-| -}
originPoint : Frame3d -> Point3d
originPoint (Types.Frame3d properties) =
    properties.originPoint


{-| -}
xDirection : Frame3d -> Direction3d
xDirection (Types.Frame3d properties) =
    properties.xDirection


{-| -}
yDirection : Frame3d -> Direction3d
yDirection (Types.Frame3d properties) =
    properties.yDirection


{-| -}
zDirection : Frame3d -> Direction3d
zDirection (Types.Frame3d properties) =
    properties.zDirection


{-| -}
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


{-| -}
xAxis : Frame3d -> Axis3d
xAxis (Types.Frame3d frame) =
    Axis3d.through frame.originPoint frame.xDirection


{-| -}
yAxis : Frame3d -> Axis3d
yAxis (Types.Frame3d frame) =
    Axis3d.through frame.originPoint frame.yDirection


{-| -}
zAxis : Frame3d -> Axis3d
zAxis (Types.Frame3d frame) =
    Axis3d.through frame.originPoint frame.zDirection


{-| -}
xyPlane : Frame3d -> Plane3d
xyPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint frame.zDirection


{-| -}
yxPlane : Frame3d -> Plane3d
yxPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint (Direction3d.reverse frame.zDirection)


{-| -}
yzPlane : Frame3d -> Plane3d
yzPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint frame.xDirection


{-| -}
zyPlane : Frame3d -> Plane3d
zyPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint (Direction3d.reverse frame.xDirection)


{-| -}
zxPlane : Frame3d -> Plane3d
zxPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint frame.yDirection


{-| -}
xzPlane : Frame3d -> Plane3d
xzPlane (Types.Frame3d frame) =
    Plane3d.through frame.originPoint (Direction3d.reverse frame.yDirection)


{-| -}
xySketchPlane : Frame3d -> SketchPlane3d
xySketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


{-| -}
yxSketchPlane : Frame3d -> SketchPlane3d
yxSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = yDirection frame
        , yDirection = xDirection frame
        }


{-| -}
yzSketchPlane : Frame3d -> SketchPlane3d
yzSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = yDirection frame
        , yDirection = zDirection frame
        }


{-| -}
zySketchPlane : Frame3d -> SketchPlane3d
zySketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = zDirection frame
        , yDirection = yDirection frame
        }


{-| -}
zxSketchPlane : Frame3d -> SketchPlane3d
zxSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = zDirection frame
        , yDirection = xDirection frame
        }


{-| -}
xzSketchPlane : Frame3d -> SketchPlane3d
xzSketchPlane frame =
    SketchPlane3d.unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = zDirection frame
        }


{-| -}
reverseX : Frame3d -> Frame3d
reverseX frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = Direction3d.reverse (xDirection frame)
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| -}
reverseY : Frame3d -> Frame3d
reverseY frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = Direction3d.reverse (yDirection frame)
        , zDirection = zDirection frame
        }


{-| -}
reverseZ : Frame3d -> Frame3d
reverseZ frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = Direction3d.reverse (zDirection frame)
        }


{-| -}
moveTo : Point3d -> Frame3d -> Frame3d
moveTo newOrigin frame =
    unsafe
        { originPoint = newOrigin
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| -}
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


{-| -}
rotateAroundOwn : (Frame3d -> Axis3d) -> Float -> Frame3d -> Frame3d
rotateAroundOwn axis angle frame =
    rotateAround (axis frame) angle frame


{-| -}
translateBy : Vector3d -> Frame3d -> Frame3d
translateBy vector frame =
    unsafe
        { originPoint = Point3d.translateBy vector (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


{-| -}
translateIn : Direction3d -> Float -> Frame3d -> Frame3d
translateIn direction distance frame =
    translateBy (Vector3d.withLength distance direction) frame


{-| -}
translateAlongOwn : (Frame3d -> Axis3d) -> Float -> Frame3d -> Frame3d
translateAlongOwn axis distance frame =
    let
        displacement =
            Vector3d.withLength distance (Axis3d.direction (axis frame))
    in
    translateBy displacement frame


{-| -}
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


{-| -}
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


{-| -}
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
