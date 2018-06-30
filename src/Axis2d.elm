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


module Axis2d
    exposing
        ( Axis2d
        , direction
        , mirrorAcross
        , moveTo
        , originPoint
        , placeIn
        , relativeTo
        , reverse
        , rotateAround
        , through
        , translateBy
        , translateIn
        , withDirection
        , x
        , y
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Axis2d/icon.svg" alt="Axis2d" width="160">

An `Axis2d` represents an infinitely long straight line in 2D and is defined by
an origin point and direction. Axes have several uses, such as:

  - Mirroring across the axis
  - Projecting onto the axis
  - Measuring distance along the axis from the origin point

@docs Axis2d


# Constants

@docs x, y


# Constructors

@docs through, withDirection


# Properties

@docs originPoint, direction


# Transformations

@docs reverse, moveTo, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

Functions for transforming axes between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn

-}

import Direction2d exposing (Direction2d)
import Geometry.Types as Types exposing (Frame2d)
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Axis2d =
    Types.Axis2d


{-| -}
x : Axis2d
x =
    through Point2d.origin Direction2d.x


{-| -}
y : Axis2d
y =
    through Point2d.origin Direction2d.y


{-| -}
through : Point2d -> Direction2d -> Axis2d
through point direction_ =
    Types.Axis2d { originPoint = point, direction = direction_ }


{-| -}
withDirection : Direction2d -> Point2d -> Axis2d
withDirection direction_ originPoint_ =
    Types.Axis2d { originPoint = originPoint_, direction = direction_ }


{-| -}
originPoint : Axis2d -> Point2d
originPoint (Types.Axis2d axis) =
    axis.originPoint


{-| -}
direction : Axis2d -> Direction2d
direction (Types.Axis2d axis) =
    axis.direction


{-| -}
reverse : Axis2d -> Axis2d
reverse (Types.Axis2d axis) =
    through axis.originPoint (Direction2d.reverse axis.direction)


{-| -}
moveTo : Point2d -> Axis2d -> Axis2d
moveTo newOrigin (Types.Axis2d axis) =
    through newOrigin axis.direction


{-| -}
rotateAround : Point2d -> Float -> Axis2d -> Axis2d
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
    \(Types.Axis2d axis) ->
        through (rotatePoint axis.originPoint) (rotateDirection axis.direction)


{-| -}
translateBy : Vector2d -> Axis2d -> Axis2d
translateBy vector (Types.Axis2d axis) =
    through (Point2d.translateBy vector axis.originPoint) axis.direction


{-| -}
translateIn : Direction2d -> Float -> Axis2d -> Axis2d
translateIn translationDirection distance axis =
    translateBy (Vector2d.withLength distance translationDirection) axis


{-| -}
mirrorAcross : Axis2d -> Axis2d -> Axis2d
mirrorAcross otherAxis (Types.Axis2d axis) =
    through (Point2d.mirrorAcross otherAxis axis.originPoint)
        (Direction2d.mirrorAcross otherAxis axis.direction)


{-| -}
relativeTo : Frame2d -> Axis2d -> Axis2d
relativeTo frame (Types.Axis2d axis) =
    through (Point2d.relativeTo frame axis.originPoint)
        (Direction2d.relativeTo frame axis.direction)


{-| -}
placeIn : Frame2d -> Axis2d -> Axis2d
placeIn frame (Types.Axis2d axis) =
    through (Point2d.placeIn frame axis.originPoint)
        (Direction2d.placeIn frame axis.direction)
