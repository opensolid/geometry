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
[`Triangle3d.on`](Triangle3d#on) returns a `Triangle3d`.

@docs SketchPlane3d


# Constants

These predefined sketch planes all have the global origin point as their origin
point, and use the two indicated global axes as their X and Y axes.

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


{-| -}
xy : SketchPlane3d
xy =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        }


{-| -}
yx : SketchPlane3d
yx =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.x
        }


{-| -}
yz : SketchPlane3d
yz =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.z
        }


{-| -}
zy : SketchPlane3d
zy =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.y
        }


{-| -}
zx : SketchPlane3d
zx =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.x
        }


{-| -}
xz : SketchPlane3d
xz =
    unsafe
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.z
        }


{-| -}
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


{-| -}
on : SketchPlane3d -> Frame2d -> SketchPlane3d
on sketchPlane frame =
    unsafe
        { originPoint = Point3d.on sketchPlane (Frame2d.originPoint frame)
        , xDirection = Direction3d.on sketchPlane (Frame2d.xDirection frame)
        , yDirection = Direction3d.on sketchPlane (Frame2d.yDirection frame)
        }


{-| -}
fromPlane : Plane3d -> SketchPlane3d
fromPlane plane =
    withNormalDirection (Plane3d.normalDirection plane)
        (Plane3d.originPoint plane)


{-| -}
unsafe : { originPoint : Point3d, xDirection : Direction3d, yDirection : Direction3d } -> SketchPlane3d
unsafe =
    Types.SketchPlane3d


{-| -}
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


{-| -}
originPoint : SketchPlane3d -> Point3d
originPoint (Types.SketchPlane3d properties) =
    properties.originPoint


{-| -}
xDirection : SketchPlane3d -> Direction3d
xDirection (Types.SketchPlane3d properties) =
    properties.xDirection


{-| -}
yDirection : SketchPlane3d -> Direction3d
yDirection (Types.SketchPlane3d properties) =
    properties.yDirection


{-| -}
normalDirection : SketchPlane3d -> Direction3d
normalDirection sketchPlane =
    let
        normalVector =
            Vector3d.crossProduct
                (Direction3d.toVector (xDirection sketchPlane))
                (Direction3d.toVector (yDirection sketchPlane))
    in
    Direction3d.unsafe (Vector3d.components normalVector)


{-| -}
xAxis : SketchPlane3d -> Axis3d
xAxis (Types.SketchPlane3d sketchPlane) =
    Axis3d.through sketchPlane.originPoint sketchPlane.xDirection


{-| -}
yAxis : SketchPlane3d -> Axis3d
yAxis (Types.SketchPlane3d sketchPlane) =
    Axis3d.through sketchPlane.originPoint sketchPlane.yDirection


{-| -}
normalAxis : SketchPlane3d -> Axis3d
normalAxis sketchPlane =
    Axis3d.through (originPoint sketchPlane) (normalDirection sketchPlane)


{-| -}
toPlane : SketchPlane3d -> Plane3d
toPlane sketchPlane =
    Plane3d.through (originPoint sketchPlane) (normalDirection sketchPlane)


{-| -}
offsetBy : Float -> SketchPlane3d -> SketchPlane3d
offsetBy distance sketchPlane =
    let
        displacement =
            Vector3d.withLength distance (normalDirection sketchPlane)
    in
    translateBy displacement sketchPlane


{-| -}
reverseX : SketchPlane3d -> SketchPlane3d
reverseX sketchPlane =
    unsafe
        { originPoint = originPoint sketchPlane
        , xDirection = Direction3d.reverse (xDirection sketchPlane)
        , yDirection = yDirection sketchPlane
        }


{-| -}
reverseY : SketchPlane3d -> SketchPlane3d
reverseY sketchPlane =
    unsafe
        { originPoint = originPoint sketchPlane
        , xDirection = xDirection sketchPlane
        , yDirection = Direction3d.reverse (yDirection sketchPlane)
        }


{-| -}
moveTo : Point3d -> SketchPlane3d -> SketchPlane3d
moveTo newOrigin sketchPlane =
    unsafe
        { originPoint = newOrigin
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


{-| -}
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


{-| -}
rotateAroundOwn : (SketchPlane3d -> Axis3d) -> Float -> SketchPlane3d -> SketchPlane3d
rotateAroundOwn axis angle sketchPlane =
    rotateAround (axis sketchPlane) angle sketchPlane


{-| -}
translateBy : Vector3d -> SketchPlane3d -> SketchPlane3d
translateBy vector sketchPlane =
    unsafe
        { originPoint = Point3d.translateBy vector (originPoint sketchPlane)
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


{-| -}
translateIn : Direction3d -> Float -> SketchPlane3d -> SketchPlane3d
translateIn direction distance sketchPlane =
    translateBy (Vector3d.withLength distance direction) sketchPlane


{-| -}
translateAlongOwn : (SketchPlane3d -> Axis3d) -> Float -> SketchPlane3d -> SketchPlane3d
translateAlongOwn axis distance frame =
    let
        displacement =
            Vector3d.withLength distance (Axis3d.direction (axis frame))
    in
    translateBy displacement frame


{-| -}
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


{-| -}
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


{-| -}
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
