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


module Frame2d
    exposing
        ( Frame2d
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
        , rotateAround
        , rotateBy
        , translateAlongOwn
        , translateBy
        , translateIn
        , unsafe
        , withXDirection
        , withYDirection
        , xAxis
        , xDirection
        , xy
        , yAxis
        , yDirection
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Frame2d/icon.svg" alt="Frame2d" width="160">

A `Frame2d` has an origin point and a pair of X and Y directions (which are
always perpendicular to each other). It can be thought of as:

  - A local coordinate system: Most geometric types have associated `relativeTo`
    and `placeIn` functions that convert values of that type from global
    coordinates to local coordinates in a particular frame, and vice versa.
  - A pair of X and Y axes: It is often convenient to (for example) mirror
    across the X axis of a frame, or project onto its Y axis. Frames can
    also themselves be translated, rotated and mirrored!
  - A combined 2D position and orientation: For example, a `Frame2d` could be
    used to define the position and orientation of a spaceship in a 2D game.
    Movement of the ship would then be done by translating and rotating the
    frame.

@docs Frame2d


# Constants

@docs xy


# Constructors

@docs atPoint, atCoordinates, withXDirection, withYDirection, unsafe


# Properties

@docs originPoint, xDirection, yDirection, isRightHanded, xAxis, yAxis


# Transformations

@docs reverseX, reverseY, moveTo, rotateBy, rotateAround, translateBy, translateIn, translateAlongOwn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis2d exposing (Axis2d)
import Direction2d exposing (Direction2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Frame2d =
    Types.Frame2d


{-| -}
xy : Frame2d
xy =
    atPoint Point2d.origin


{-| -}
withXDirection : Direction2d -> Point2d -> Frame2d
withXDirection xDirection_ originPoint_ =
    unsafe
        { originPoint = originPoint_
        , xDirection = xDirection_
        , yDirection = xDirection_ |> Direction2d.rotateCounterclockwise
        }


{-| -}
withYDirection : Direction2d -> Point2d -> Frame2d
withYDirection yDirection_ originPoint_ =
    unsafe
        { originPoint = originPoint_
        , xDirection = yDirection_ |> Direction2d.rotateClockwise
        , yDirection = yDirection_
        }


{-| -}
unsafe : { originPoint : Point2d, xDirection : Direction2d, yDirection : Direction2d } -> Frame2d
unsafe =
    Types.Frame2d


{-| -}
atPoint : Point2d -> Frame2d
atPoint point =
    unsafe
        { originPoint = point
        , xDirection = Direction2d.x
        , yDirection = Direction2d.y
        }


{-| -}
atCoordinates : ( Float, Float ) -> Frame2d
atCoordinates coordinates =
    atPoint (Point2d.fromCoordinates coordinates)


{-| -}
originPoint : Frame2d -> Point2d
originPoint (Types.Frame2d frame) =
    frame.originPoint


{-| -}
xDirection : Frame2d -> Direction2d
xDirection (Types.Frame2d frame) =
    frame.xDirection


{-| -}
yDirection : Frame2d -> Direction2d
yDirection (Types.Frame2d frame) =
    frame.yDirection


{-| -}
isRightHanded : Frame2d -> Bool
isRightHanded frame =
    let
        xVector =
            Direction2d.toVector (xDirection frame)

        yVector =
            Direction2d.toVector (yDirection frame)
    in
    Vector2d.crossProduct xVector yVector > 0


{-| -}
xAxis : Frame2d -> Axis2d
xAxis (Types.Frame2d frame) =
    Axis2d.through frame.originPoint frame.xDirection


{-| -}
yAxis : Frame2d -> Axis2d
yAxis (Types.Frame2d frame) =
    Axis2d.through frame.originPoint frame.yDirection


{-| -}
reverseX : Frame2d -> Frame2d
reverseX frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = Direction2d.reverse (xDirection frame)
        , yDirection = yDirection frame
        }


{-| -}
reverseY : Frame2d -> Frame2d
reverseY frame =
    unsafe
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = Direction2d.reverse (yDirection frame)
        }


{-| -}
moveTo : Point2d -> Frame2d -> Frame2d
moveTo newOrigin frame =
    unsafe
        { originPoint = newOrigin
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


{-| -}
rotateBy : Float -> Frame2d -> Frame2d
rotateBy angle frame =
    let
        rotateDirection =
            Direction2d.rotateBy angle
    in
    unsafe
        { originPoint = originPoint frame
        , xDirection = rotateDirection (xDirection frame)
        , yDirection = rotateDirection (yDirection frame)
        }


{-| -}
rotateAround : Point2d -> Float -> Frame2d -> Frame2d
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
    \frame ->
        unsafe
            { originPoint = rotatePoint (originPoint frame)
            , xDirection = rotateDirection (xDirection frame)
            , yDirection = rotateDirection (yDirection frame)
            }


{-| -}
translateBy : Vector2d -> Frame2d -> Frame2d
translateBy vector frame =
    unsafe
        { originPoint = Point2d.translateBy vector (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


{-| -}
translateIn : Direction2d -> Float -> Frame2d -> Frame2d
translateIn direction distance frame =
    translateBy (Vector2d.withLength distance direction) frame


{-| -}
translateAlongOwn : (Frame2d -> Axis2d) -> Float -> Frame2d -> Frame2d
translateAlongOwn axis distance frame =
    let
        displacement =
            Vector2d.withLength distance (Axis2d.direction (axis frame))
    in
    translateBy displacement frame


{-| -}
mirrorAcross : Axis2d -> Frame2d -> Frame2d
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis

        mirrorDirection =
            Direction2d.mirrorAcross axis
    in
    \frame ->
        unsafe
            { originPoint = mirrorPoint (originPoint frame)
            , xDirection = mirrorDirection (xDirection frame)
            , yDirection = mirrorDirection (yDirection frame)
            }


{-| -}
relativeTo : Frame2d -> Frame2d -> Frame2d
relativeTo otherFrame =
    let
        relativePoint =
            Point2d.relativeTo otherFrame

        relativeDirection =
            Direction2d.relativeTo otherFrame
    in
    \frame ->
        unsafe
            { originPoint = relativePoint (originPoint frame)
            , xDirection = relativeDirection (xDirection frame)
            , yDirection = relativeDirection (yDirection frame)
            }


{-| -}
placeIn : Frame2d -> Frame2d -> Frame2d
placeIn otherFrame =
    let
        placePoint =
            Point2d.placeIn otherFrame

        placeDirection =
            Direction2d.placeIn otherFrame
    in
    \frame ->
        unsafe
            { originPoint = placePoint (originPoint frame)
            , xDirection = placeDirection (xDirection frame)
            , yDirection = placeDirection (yDirection frame)
            }
