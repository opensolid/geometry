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


module Plane3d
    exposing
        ( Plane3d
        , mirrorAcross
        , moveTo
        , normalAxis
        , normalDirection
        , offsetBy
        , originPoint
        , placeIn
        , relativeTo
        , reverseNormal
        , rotateAround
        , through
        , throughPoints
        , translateBy
        , translateIn
        , withNormalDirection
        , xy
        , yz
        , zx
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Plane3d/icon.svg" alt="Plane3d" width="160">

A `Plane3d` is an infinite flat plane in 3D. It is defined by an origin point
and normal direction and is useful for several operations including:

  - Mirroring across the plane
  - Projecting onto the plane
  - Measuring distance from the plane

@docs Plane3d


# Constants

@docs xy, yz, zx


# Constructors

@docs through, withNormalDirection, throughPoints


# Properties

@docs originPoint, normalDirection, normalAxis


# Transformations

@docs offsetBy, reverseNormal, rotateAround, translateBy, translateIn, moveTo, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Geometry.Types as Types exposing (Frame3d)
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Plane3d =
    Types.Plane3d


{-| -}
xy : Plane3d
xy =
    through Point3d.origin Direction3d.z


{-| -}
yz : Plane3d
yz =
    through Point3d.origin Direction3d.x


{-| -}
zx : Plane3d
zx =
    through Point3d.origin Direction3d.y


{-| -}
through : Point3d -> Direction3d -> Plane3d
through point normalDirection_ =
    Types.Plane3d
        { originPoint = point
        , normalDirection = normalDirection_
        }


{-| -}
withNormalDirection : Direction3d -> Point3d -> Plane3d
withNormalDirection normalDirection_ originPoint_ =
    Types.Plane3d
        { normalDirection = normalDirection_
        , originPoint = originPoint_
        }


{-| -}
throughPoints : Point3d -> Point3d -> Point3d -> Maybe Plane3d
throughPoints firstPoint secondPoint thirdPoint =
    let
        firstVector =
            Vector3d.from firstPoint secondPoint

        secondVector =
            Vector3d.from secondPoint thirdPoint

        crossProduct =
            Vector3d.crossProduct firstVector secondVector
    in
    Vector3d.direction crossProduct |> Maybe.map (through firstPoint)


{-| -}
originPoint : Plane3d -> Point3d
originPoint (Types.Plane3d plane) =
    plane.originPoint


{-| -}
normalDirection : Plane3d -> Direction3d
normalDirection (Types.Plane3d plane) =
    plane.normalDirection


{-| -}
normalAxis : Plane3d -> Axis3d
normalAxis (Types.Plane3d plane) =
    Axis3d.through plane.originPoint plane.normalDirection


{-| -}
offsetBy : Float -> Plane3d -> Plane3d
offsetBy distance plane =
    let
        displacement =
            Vector3d.withLength distance (normalDirection plane)
    in
    translateBy displacement plane


{-| -}
reverseNormal : Plane3d -> Plane3d
reverseNormal (Types.Plane3d plane) =
    through plane.originPoint (Direction3d.reverse plane.normalDirection)


{-| -}
rotateAround : Axis3d -> Float -> Plane3d -> Plane3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
    \(Types.Plane3d plane) ->
        through (rotatePoint plane.originPoint)
            (rotateDirection plane.normalDirection)


{-| -}
translateBy : Vector3d -> Plane3d -> Plane3d
translateBy vector (Types.Plane3d plane) =
    withNormalDirection plane.normalDirection
        (Point3d.translateBy vector plane.originPoint)


{-| -}
translateIn : Direction3d -> Float -> Plane3d -> Plane3d
translateIn direction distance plane =
    translateBy (Vector3d.withLength distance direction) plane


{-| -}
moveTo : Point3d -> Plane3d -> Plane3d
moveTo newOrigin (Types.Plane3d plane) =
    through newOrigin plane.normalDirection


{-| -}
mirrorAcross : Plane3d -> Plane3d -> Plane3d
mirrorAcross otherPlane (Types.Plane3d plane) =
    through (Point3d.mirrorAcross otherPlane plane.originPoint)
        (Direction3d.mirrorAcross otherPlane plane.normalDirection)


{-| -}
relativeTo : Frame3d -> Plane3d -> Plane3d
relativeTo frame (Types.Plane3d plane) =
    through (Point3d.relativeTo frame plane.originPoint)
        (Direction3d.relativeTo frame plane.normalDirection)


{-| -}
placeIn : Frame3d -> Plane3d -> Plane3d
placeIn frame (Types.Plane3d plane) =
    through (Point3d.placeIn frame plane.originPoint)
        (Direction3d.placeIn frame plane.normalDirection)
