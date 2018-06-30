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


module Axis3d
    exposing
        ( Axis3d
        , direction
        , mirrorAcross
        , moveTo
        , on
        , originPoint
        , placeIn
        , projectInto
        , projectOnto
        , relativeTo
        , reverse
        , rotateAround
        , through
        , translateBy
        , translateIn
        , withDirection
        , x
        , y
        , z
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Axis3d/icon.svg" alt="Axis3d" width="160">

An `Axis3d` represents an infinitely long straight line in 3D and is defined by
an origin point and direction. Axes have several uses, such as:

  - Rotating around the axis
  - Projecting onto the axis
  - Measuring distance along the axis from the origin point

@docs Axis3d


# Constants

@docs x, y, z


# Constructors

@docs through, withDirection, on


# Properties

@docs originPoint, direction


# Transformations

@docs reverse, moveTo, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto


# Coordinate conversions

Functions for transforming axes between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn, projectInto

-}

import Axis2d exposing (Axis2d)
import Direction3d exposing (Direction3d)
import Geometry.Types as Types exposing (Frame3d, Plane3d, SketchPlane3d)
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Axis3d =
    Types.Axis3d


{-| -}
x : Axis3d
x =
    through Point3d.origin Direction3d.x


{-| -}
y : Axis3d
y =
    through Point3d.origin Direction3d.y


{-| -}
z : Axis3d
z =
    through Point3d.origin Direction3d.z


{-| -}
through : Point3d -> Direction3d -> Axis3d
through point direction_ =
    Types.Axis3d { originPoint = point, direction = direction_ }


{-| -}
withDirection : Direction3d -> Point3d -> Axis3d
withDirection direction_ originPoint_ =
    Types.Axis3d { direction = direction_, originPoint = originPoint_ }


{-| -}
on : SketchPlane3d -> Axis2d -> Axis3d
on sketchPlane (Types.Axis2d axis2d) =
    through (Point3d.on sketchPlane axis2d.originPoint)
        (Direction3d.on sketchPlane axis2d.direction)


{-| -}
originPoint : Axis3d -> Point3d
originPoint (Types.Axis3d axis) =
    axis.originPoint


{-| -}
direction : Axis3d -> Direction3d
direction (Types.Axis3d axis) =
    axis.direction


{-| -}
reverse : Axis3d -> Axis3d
reverse (Types.Axis3d axis) =
    through axis.originPoint (Direction3d.reverse axis.direction)


{-| -}
moveTo : Point3d -> Axis3d -> Axis3d
moveTo newOrigin (Types.Axis3d axis) =
    through newOrigin axis.direction


{-| -}
rotateAround : Axis3d -> Float -> Axis3d -> Axis3d
rotateAround otherAxis angle =
    let
        rotatePoint =
            Point3d.rotateAround otherAxis angle

        rotateDirection =
            Direction3d.rotateAround otherAxis angle
    in
    \(Types.Axis3d axis) ->
        through (rotatePoint axis.originPoint) (rotateDirection axis.direction)


{-| -}
translateBy : Vector3d -> Axis3d -> Axis3d
translateBy vector (Types.Axis3d axis) =
    through (Point3d.translateBy vector axis.originPoint) axis.direction


{-| -}
translateIn : Direction3d -> Float -> Axis3d -> Axis3d
translateIn translationDirection distance axis =
    translateBy (Vector3d.withLength distance translationDirection) axis


{-| -}
mirrorAcross : Plane3d -> Axis3d -> Axis3d
mirrorAcross plane (Types.Axis3d axis) =
    through (Point3d.mirrorAcross plane axis.originPoint)
        (Direction3d.mirrorAcross plane axis.direction)


{-| -}
projectOnto : Plane3d -> Axis3d -> Maybe Axis3d
projectOnto plane (Types.Axis3d axis) =
    let
        projectedOrigin =
            Point3d.projectOnto plane axis.originPoint
    in
    Direction3d.projectOnto plane axis.direction
        |> Maybe.map (through projectedOrigin)


{-| -}
relativeTo : Frame3d -> Axis3d -> Axis3d
relativeTo frame (Types.Axis3d axis) =
    through (Point3d.relativeTo frame axis.originPoint)
        (Direction3d.relativeTo frame axis.direction)


{-| -}
placeIn : Frame3d -> Axis3d -> Axis3d
placeIn frame (Types.Axis3d axis) =
    through (Point3d.placeIn frame axis.originPoint)
        (Direction3d.placeIn frame axis.direction)


{-| -}
projectInto : SketchPlane3d -> Axis3d -> Maybe Axis2d
projectInto sketchPlane (Types.Axis3d axis) =
    let
        projectedOrigin =
            Point3d.projectInto sketchPlane axis.originPoint
    in
    Direction3d.projectInto sketchPlane axis.direction
        |> Maybe.map (Axis2d.through projectedOrigin)
