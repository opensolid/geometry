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


{-| The global XY plane, centered at the origin with a normal in the positive Z
direction.

    Plane3d.xy
    --> Plane3d.through Point3d.origin Direction3d.z

-}
xy : Plane3d
xy =
    through Point3d.origin Direction3d.z


{-| The global YZ plane, centered at the origin with a normal in the positive X
direction.

    Plane3d.yz
    --> Plane3d.through Point3d.origin Direction3d.x

-}
yz : Plane3d
yz =
    through Point3d.origin Direction3d.x


{-| The global ZX plane, centered at the origin with a normal in the positive Y
direction.

    Plane3d.zx
    --> through Point3d.origin Direction3d.y

-}
zx : Plane3d
zx =
    through Point3d.origin Direction3d.y


{-| Construct a plane through the given point, with the given normal direction.

    xyPlane =
        Plane3d.through Point3d.origin Direction3d.z

-}
through : Point3d -> Direction3d -> Plane3d
through point normalDirection_ =
    Types.Plane3d
        { originPoint = point
        , normalDirection = normalDirection_
        }


{-| Construct a plane with the given normal direction, through the given point.
Flipped version of `through`.

    plane =
        Plane3d.withNormalDirection Direction3d.y
            (Point3d.fromCoordinates ( 2, 1, 3 ))

-}
withNormalDirection : Direction3d -> Point3d -> Plane3d
withNormalDirection normalDirection_ originPoint_ =
    Types.Plane3d
        { normalDirection = normalDirection_
        , originPoint = originPoint_
        }


{-| Attempt to construct a plane passing through the three given points. The
origin point of the resulting plane will be equal to the first given point, and
the normal direction will be such that the three given points are in
counterclockwise order around it according to the right-hand rule. If the three
given points are collinear, returns `Nothing`.

    Plane3d.throughPoints
        (Point3d.fromCoordinates ( 2, 0, 0 ))
        (Point3d.fromCoordinates ( 3, 0, 0 ))
        (Point3d.fromCoordinates ( 4, 1, 1 ))
    --> Just
    -->     (Plane3d.through
    -->         (Point3d.fromCoordinates ( 2, 0, 0 ))
    -->         (Direction3d.fromAzimuthAndElevation
    -->             (degrees -90)
    -->             (degrees 45)
    -->         )
    -->     )

    Plane3d.throughPoints
        (Point3d.fromCoordinates ( 2, 0, 0 ))
        (Point3d.fromCoordinates ( 3, 0, 0 ))
        (Point3d.fromCoordinates ( 4, 0, 0 ))
    --> Nothing

-}
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


{-| Get the origin point of a plane.

    Plane3d.originPoint Plane3d.xy
    --> Point3d.origin

-}
originPoint : Plane3d -> Point3d
originPoint (Types.Plane3d plane) =
    plane.originPoint


{-| Get the normal direction of a plane.

    Plane3d.normalDirection Plane3d.xy
    --> Direction3d.z

-}
normalDirection : Plane3d -> Direction3d
normalDirection (Types.Plane3d plane) =
    plane.normalDirection


{-| Construct an axis from the origin point and normal direction of a plane.

    Plane3d.normalAxis Plane3d.zx
    --> Axis3d.y

-}
normalAxis : Plane3d -> Axis3d
normalAxis (Types.Plane3d plane) =
    Axis3d.through plane.originPoint plane.normalDirection


{-| Shift a plane in its own normal direction by the given (signed) distance.

    Plane3d.offsetBy 1.0 Plane3d.zx
    --> Plane3d.withNormalDirection Direction3d.y
    -->     (Point3d.fromCoordinates ( 0, 1, 0 ))

    Plane3d.offsetBy -2.0 Plane3d.xy
    --> Plane3d.withNormalDirection Direction3d.z
    -->     (Point3d.fromCoordinates ( 0, 0, -2 ))

-}
offsetBy : Float -> Plane3d -> Plane3d
offsetBy distance plane =
    let
        displacement =
            Vector3d.withLength distance (normalDirection plane)
    in
    translateBy displacement plane


{-| Reverse a plane's normal direction while leaving its origin point unchanged.

    Plane3d.reverseNormal Plane3d.xy
    --> Plane3d.through Point3d.origin
    -->     Direction3d.negativeZ

-}
reverseNormal : Plane3d -> Plane3d
reverseNormal (Types.Plane3d plane) =
    through plane.originPoint (Direction3d.reverse plane.normalDirection)


{-| Rotate a plane around an axis by a given angle.

    Plane3d.rotateAround Axis3d.y (degrees 90) Plane3d.xy
    --> Plane3d.yz

-}
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


{-| Translate a plane by a given displacement. Applies the given displacement to
the plane's origin point and leaves its normal direction unchanged.

    plane =
        Plane3d.withNormalDirection Direction3d.z
            (Point3d.fromCoordinates ( 1, 1, 1 ))

    displacement =
        Vector3d.fromComponents ( 1, 2, 3 )

    Plane3d.translateBy displacement plane
    --> Plane3d.withNormalDirection Direction3d.z
    -->     (Point3d.fromCoordinates ( 2, 3, 4 ))

-}
translateBy : Vector3d -> Plane3d -> Plane3d
translateBy vector (Types.Plane3d plane) =
    withNormalDirection plane.normalDirection
        (Point3d.translateBy vector plane.originPoint)


{-| Translate a plane in a given direction by a given distance;

    Plane3d.translateIn direction distance

is equivalent to

    Plane3d.translateBy
        (Vector3d.withLength distance direction)

-}
translateIn : Direction3d -> Float -> Plane3d -> Plane3d
translateIn direction distance plane =
    translateBy (Vector3d.withLength distance direction) plane


{-| Move a plane so that it has the given origin point but unchanged normal
direction.

    newOrigin =
        Point3d.fromCoordinates ( 1, 2, 3 )

    Plane3d.moveTo newOrigin Plane3d.xy
    --> Plane3d.through newOrigin Direction3d.z

-}
moveTo : Point3d -> Plane3d -> Plane3d
moveTo newOrigin (Types.Plane3d plane) =
    through newOrigin plane.normalDirection


{-| Mirror one plane across another. The plane to mirror across is given first
and the plane to mirror is given second.

    plane =
        Plane3d.withNormalDirection Direction3d.z
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Plane3d.mirrorAcross Plane3d.xy plane
    --> Plane3d.withNormalDirection Direction3d.negativeZ
    -->     (Point3d.fromCoordinates ( 1, 2, -3 ))

-}
mirrorAcross : Plane3d -> Plane3d -> Plane3d
mirrorAcross otherPlane (Types.Plane3d plane) =
    through (Point3d.mirrorAcross otherPlane plane.originPoint)
        (Direction3d.mirrorAcross otherPlane plane.normalDirection)


{-| Take a plane defined in global coordinates, and return it expressed in local
coordinates relative to a given reference frame.

    referenceFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 1, 1 ))

    plane =
        Plane3d.withNormalDirection Direction3d.z
            (Point3d.fromCoordinates ( 0, 0, 2 ))

    Plane3d.relativeTo referenceFrame plane
    --> Plane3d.withNormalDirection Direction3d.z
    -->     (Point3d.fromCoordinates ( -1, -1, 1 ))

-}
relativeTo : Frame3d -> Plane3d -> Plane3d
relativeTo frame (Types.Plane3d plane) =
    through (Point3d.relativeTo frame plane.originPoint)
        (Direction3d.relativeTo frame plane.normalDirection)


{-| Take a plane defined in local coordinates relative to a given reference
frame, and return that plane expressed in global coordinates.

    referenceFrame =
        Frame3d.atPoint
            (Point3d.fromCoordinates ( 1, 1, 1 ))

    plane =
        Plane3d.withNormalDirection Direction3d.z
            (Point3d.fromCoordinates ( 1, 2, 3 ))

    Plane3d.placeIn referenceFrame plane
    --> Plane3d.withNormalDirection Direction3d.z
    -->     (Point3d.fromCoordinates ( 2, 3, 4 ))

-}
placeIn : Frame3d -> Plane3d -> Plane3d
placeIn frame (Types.Plane3d plane) =
    through (Point3d.placeIn frame plane.originPoint)
        (Direction3d.placeIn frame plane.normalDirection)
