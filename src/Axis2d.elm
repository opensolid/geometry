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


{-| The global X axis.

    Axis2d.x
    --> Axis2d.through Point2d.origin Direction2d.x

-}
x : Axis2d
x =
    through Point2d.origin Direction2d.x


{-| The global Y axis.

    Axis2d.y
    --> Axis2d.through Point2d.origin Direction2d.y

-}
y : Axis2d
y =
    through Point2d.origin Direction2d.y


{-| Construct an axis through the given origin point with the given direction.

    exampleAxis =
        Axis2d.through (Point2d.fromCoordinates ( 1, 3 ))
            (Direction2d.fromAngle (degrees 30))

-}
through : Point2d -> Direction2d -> Axis2d
through point direction_ =
    Types.Axis2d { originPoint = point, direction = direction_ }


{-| Construct an axis with the given direction, through the given origin point.
Flipped version of `through`. Having both versions allow you to do different
things with partial application:

    -- A list of axes in different directions all passing
    -- through the same origin point
    List.map (Axis2d.through point) directions

    -- A list of parallel axes (all having the same
    -- direction) through different points
    List.map (Axis2d.withDirection direction) points

-}
withDirection : Direction2d -> Point2d -> Axis2d
withDirection direction_ originPoint_ =
    Types.Axis2d { originPoint = originPoint_, direction = direction_ }


{-| Get the origin point of an axis.

    Axis2d.originPoint exampleAxis
    --> Point2d.fromCoordinates ( 1, 3 )

-}
originPoint : Axis2d -> Point2d
originPoint (Types.Axis2d axis) =
    axis.originPoint


{-| Get the direction of an axis.

    Axis2d.direction exampleAxis
    --> Direction2d.fromAngle (degrees 30)

-}
direction : Axis2d -> Direction2d
direction (Types.Axis2d axis) =
    axis.direction


{-| Reverse the direction of an axis while keeping the same origin point.

    Axis2d.reverse exampleAxis
    --> Axis2d.through (Point2d.fromCoordinates ( 1, 3 ))
    -->     (Direction2d.fromAngle (degrees -150))

-}
reverse : Axis2d -> Axis2d
reverse (Types.Axis2d axis) =
    through axis.originPoint (Direction2d.reverse axis.direction)


{-| Move an axis so that it has the given origin point but unchanged direction.

    newOrigin =
        Point2d.fromCoordinates ( 4, 5 )

    Axis2d.moveTo newOrigin exampleAxis
    --> Axis2d.through (Point2d.fromCoordinates ( 4, 5 ))
    -->     (Direction2d.fromAngle (degrees 30))

-}
moveTo : Point2d -> Axis2d -> Axis2d
moveTo newOrigin (Types.Axis2d axis) =
    through newOrigin axis.direction


{-| Rotate an axis around a given center point by a given angle. Rotates the
axis' origin point around the given point by the given angle and the axis'
direction by the given angle.

    exampleAxis
        |> Axis2d.rotateAround Point2d.origin (degrees 90)
    --> Axis2d.through (Point2d.fromCoordinates ( -3, 1 ))
    -->     (Direction2d.fromAngle (degrees 120))

-}
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


{-| Translate an axis by a given displacement. Applies the given displacement to
the axis' origin point and leaves the direction unchanged.

    displacement =
        Vector2d.fromComponents ( 2, 3 )

    Axis2d.translateBy displacement exampleAxis
    --> Axis2d.through (Point2d.fromCoordinates ( 3, 6 ))
    -->     (Direction2d.fromAngle (degrees 30))

-}
translateBy : Vector2d -> Axis2d -> Axis2d
translateBy vector (Types.Axis2d axis) =
    through (Point2d.translateBy vector axis.originPoint) axis.direction


{-| Translate an axis in a given direction by a given distance;

    Axis2d.translateIn direction distance

is equivalent to

    Axis2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> Axis2d -> Axis2d
translateIn translationDirection distance axis =
    translateBy (Vector2d.withLength distance translationDirection) axis


{-| Mirror one axis across another. The axis to mirror across is given first and
the axis to mirror is given second.

    Axis2d.mirrorAcross Axis2d.x exampleAxis
    --> Axis2d.through (Point2d.fromCoordinates ( 1, -3 ))
    -->     (Direction2d.fromAngle (degrees -30))

-}
mirrorAcross : Axis2d -> Axis2d -> Axis2d
mirrorAcross otherAxis (Types.Axis2d axis) =
    through (Point2d.mirrorAcross otherAxis axis.originPoint)
        (Direction2d.mirrorAcross otherAxis axis.direction)


{-| Take an axis defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    frame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    Axis2d.relativeTo frame exampleAxis
    --> Axis2d.through (Point2d.fromCoordinates ( -1, 0 ))
    -->     (Direction2d.fromAngle (degrees 30))

-}
relativeTo : Frame2d -> Axis2d -> Axis2d
relativeTo frame (Types.Axis2d axis) =
    through (Point2d.relativeTo frame axis.originPoint)
        (Direction2d.relativeTo frame axis.direction)


{-| Take an axis defined in local coordinates relative to a given reference
frame, and return that axis expressed in global coordinates.

    frame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 2, 3 ))

    Axis2d.placeIn frame exampleAxis
    --> Axis2d.through (Point2d.fromCoordinates ( 3, 6 ))
    -->     (Direction2d.fromAngle (degrees 30))

-}
placeIn : Frame2d -> Axis2d -> Axis2d
placeIn frame (Types.Axis2d axis) =
    through (Point2d.placeIn frame axis.originPoint)
        (Direction2d.placeIn frame axis.direction)
