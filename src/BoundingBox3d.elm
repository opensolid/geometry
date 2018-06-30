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


module BoundingBox3d
    exposing
        ( BoundingBox3d
        , aggregate
        , centroid
        , containingPoints
        , contains
        , dimensions
        , extrema
        , from
        , fromExtrema
        , hull
        , intersection
        , intersects
        , isContainedIn
        , maxX
        , maxY
        , maxZ
        , midX
        , midY
        , midZ
        , minX
        , minY
        , minZ
        , overlappingBy
        , scaleAbout
        , separatedBy
        , singleton
        , translateBy
        , translateIn
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/BoundingBox3d/icon.svg" alt="BoundingBox3d" width="160">

A `BoundingBox3d` is a rectangular box in 3D defined by its minimum and maximum
X, Y and Z values. It is possible to generate bounding boxes for most geometric
objects; for example, [`Triangle3d.boundingBox`](Triangle3d#boundingBox) takes a
`Triangle3d` and returns a `BoundingBox3d` that contains that triangle. There
are several use cases where it is more efficient to deal with the bounding box
of an object than the object itself, such as:

  - Intersection checking: If (for example) the bounding boxes of a line segment
    and a triangle do not overlap, then the line segment and triangle cannot
    possibly intersect each other. Expensive intersection checking therefore
    only has to be performed for line segments and triangles whose bounding
    boxes _do_ overlap.
  - 3D rendering: When rendering a 3D scene, any object whose bounding box is
    not visible must itself be not visible, and therefore does not have to be
    drawn. This provides a simple form of culling.

@docs BoundingBox3d


# Constructors

@docs fromExtrema, singleton, from, hull, intersection, aggregate, containingPoints


# Properties

@docs extrema, minX, maxX, minY, maxY, minZ, maxZ, dimensions, midX, midY, midZ, centroid


# Queries

@docs contains, isContainedIn, intersects, overlappingBy, separatedBy


# Transformations

@docs scaleAbout, translateBy, translateIn

-}

import Direction3d exposing (Direction3d)
import Geometry.Types as Types
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias BoundingBox3d =
    Types.BoundingBox3d


{-| -}
fromExtrema : { minX : Float, maxX : Float, minY : Float, maxY : Float, minZ : Float, maxZ : Float } -> BoundingBox3d
fromExtrema extrema_ =
    if
        (extrema_.minX <= extrema_.maxX)
            && (extrema_.minY <= extrema_.maxY)
            && (extrema_.minZ <= extrema_.maxZ)
    then
        Types.BoundingBox3d extrema_
    else
        Types.BoundingBox3d
            { minX = min extrema_.minX extrema_.maxX
            , maxX = max extrema_.minX extrema_.maxX
            , minY = min extrema_.minY extrema_.maxY
            , maxY = max extrema_.minY extrema_.maxY
            , minZ = min extrema_.minZ extrema_.maxZ
            , maxZ = max extrema_.minZ extrema_.maxZ
            }


{-| -}
singleton : Point3d -> BoundingBox3d
singleton point =
    let
        ( x, y, z ) =
            Point3d.coordinates point
    in
    fromExtrema
        { minX = x
        , maxX = x
        , minY = y
        , maxY = y
        , minZ = z
        , maxZ = z
        }


{-| -}
from : Point3d -> Point3d -> BoundingBox3d
from firstPoint secondPoint =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates firstPoint

        ( x2, y2, z2 ) =
            Point3d.coordinates secondPoint
    in
    fromExtrema
        { minX = min x1 x2
        , maxX = max x1 x2
        , minY = min y1 y2
        , maxY = max y1 y2
        , minZ = min z1 z2
        , maxZ = max z1 z2
        }


{-| -}
aggregate : List BoundingBox3d -> Maybe BoundingBox3d
aggregate boundingBoxes =
    case boundingBoxes of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| -}
containingPoints : List Point3d -> Maybe BoundingBox3d
containingPoints points =
    aggregate (List.map singleton points)


{-| -}
extrema : BoundingBox3d -> { minX : Float, maxX : Float, minY : Float, maxY : Float, minZ : Float, maxZ : Float }
extrema (Types.BoundingBox3d extrema_) =
    extrema_


{-| -}
minX : BoundingBox3d -> Float
minX (Types.BoundingBox3d boundingBox) =
    boundingBox.minX


{-| -}
maxX : BoundingBox3d -> Float
maxX (Types.BoundingBox3d boundingBox) =
    boundingBox.maxX


{-| -}
minY : BoundingBox3d -> Float
minY (Types.BoundingBox3d boundingBox) =
    boundingBox.minY


{-| -}
maxY : BoundingBox3d -> Float
maxY (Types.BoundingBox3d boundingBox) =
    boundingBox.maxY


{-| -}
minZ : BoundingBox3d -> Float
minZ (Types.BoundingBox3d boundingBox) =
    boundingBox.minZ


{-| -}
maxZ : BoundingBox3d -> Float
maxZ (Types.BoundingBox3d boundingBox) =
    boundingBox.maxZ


{-| -}
dimensions : BoundingBox3d -> ( Float, Float, Float )
dimensions boundingBox =
    ( maxX boundingBox - minX boundingBox
    , maxY boundingBox - minY boundingBox
    , maxZ boundingBox - minZ boundingBox
    )


{-| -}
midX : BoundingBox3d -> Float
midX (Types.BoundingBox3d boundingBox) =
    boundingBox.minX + 0.5 * (boundingBox.maxX - boundingBox.minX)


{-| -}
midY : BoundingBox3d -> Float
midY (Types.BoundingBox3d boundingBox) =
    boundingBox.minY + 0.5 * (boundingBox.maxY - boundingBox.minY)


{-| -}
midZ : BoundingBox3d -> Float
midZ (Types.BoundingBox3d boundingBox) =
    boundingBox.minZ + 0.5 * (boundingBox.maxZ - boundingBox.minZ)


{-| -}
centroid : BoundingBox3d -> Point3d
centroid boundingBox =
    Point3d.fromCoordinates
        ( midX boundingBox
        , midY boundingBox
        , midZ boundingBox
        )


{-| -}
contains : Point3d -> BoundingBox3d -> Bool
contains point boundingBox =
    let
        ( x, y, z ) =
            Point3d.coordinates point
    in
    (minX boundingBox <= x && x <= maxX boundingBox)
        && (minY boundingBox <= y && y <= maxY boundingBox)
        && (minZ boundingBox <= z && z <= maxZ boundingBox)


{-| -}
intersects : BoundingBox3d -> BoundingBox3d -> Bool
intersects other boundingBox =
    (minX boundingBox <= maxX other)
        && (maxX boundingBox >= minX other)
        && (minY boundingBox <= maxY other)
        && (maxY boundingBox >= minY other)
        && (minZ boundingBox <= maxZ other)
        && (maxZ boundingBox >= minZ other)


overlapAmount : BoundingBox3d -> BoundingBox3d -> Maybe Float
overlapAmount firstBox secondBox =
    let
        xOverlap =
            min (maxX firstBox) (maxX secondBox)
                - max (minX firstBox) (minX secondBox)

        yOverlap =
            min (maxY firstBox) (maxY secondBox)
                - max (minY firstBox) (minY secondBox)

        zOverlap =
            min (maxZ firstBox) (maxZ secondBox)
                - max (minZ firstBox) (minZ secondBox)
    in
    if xOverlap >= 0 && yOverlap >= 0 && zOverlap >= 0 then
        Just (min xOverlap (min yOverlap zOverlap))
    else
        Nothing


squaredSeparationAmount : BoundingBox3d -> BoundingBox3d -> Maybe Float
squaredSeparationAmount firstBox secondBox =
    let
        xSeparation =
            max (minX firstBox) (minX secondBox)
                - min (maxX firstBox) (maxX secondBox)

        ySeparation =
            max (minY firstBox) (minY secondBox)
                - min (maxY firstBox) (maxY secondBox)

        zSeparation =
            max (minZ firstBox) (minZ secondBox)
                - min (maxZ firstBox) (maxZ secondBox)
    in
    if xSeparation >= 0 || ySeparation >= 0 || zSeparation >= 0 then
        let
            dX =
                max xSeparation 0

            dY =
                max ySeparation 0

            dZ =
                max zSeparation 0
        in
        Just (dX * dX + dY * dY + dZ * dZ)
    else
        Nothing


alwaysFalse : BoundingBox3d -> BoundingBox3d -> Bool
alwaysFalse firstBox secondBox =
    False


{-| -}
overlappingBy : Order -> Float -> BoundingBox3d -> BoundingBox3d -> Bool
overlappingBy order tolerance =
    case order of
        LT ->
            if tolerance > 0 then
                \firstBox secondBox ->
                    case overlapAmount firstBox secondBox of
                        Just distance ->
                            distance < tolerance

                        Nothing ->
                            True
            else if tolerance == 0 then
                \firstBox secondBox ->
                    overlapAmount firstBox secondBox == Nothing
            else
                alwaysFalse

        GT ->
            if tolerance >= 0 then
                \firstBox secondBox ->
                    case overlapAmount firstBox secondBox of
                        Just distance ->
                            distance > tolerance

                        Nothing ->
                            False
            else
                \firstBox secondBox ->
                    overlapAmount firstBox secondBox /= Nothing

        EQ ->
            if tolerance >= 0 then
                let
                    expected =
                        Just tolerance
                in
                \firstBox secondBox ->
                    overlapAmount firstBox secondBox == expected
            else
                alwaysFalse


{-| -}
separatedBy : Order -> Float -> BoundingBox3d -> BoundingBox3d -> Bool
separatedBy order tolerance =
    case order of
        LT ->
            if tolerance > 0 then
                \firstBox secondBox ->
                    case squaredSeparationAmount firstBox secondBox of
                        Just squaredDistance ->
                            squaredDistance < tolerance * tolerance

                        Nothing ->
                            True
            else if tolerance == 0 then
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox == Nothing
            else
                alwaysFalse

        GT ->
            if tolerance >= 0 then
                \firstBox secondBox ->
                    case squaredSeparationAmount firstBox secondBox of
                        Just squaredDistance ->
                            squaredDistance > tolerance * tolerance

                        Nothing ->
                            False
            else
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox /= Nothing

        EQ ->
            if tolerance >= 0 then
                let
                    expected =
                        Just (tolerance * tolerance)
                in
                \firstBox secondBox ->
                    squaredSeparationAmount firstBox secondBox == expected
            else
                alwaysFalse


{-| -}
isContainedIn : BoundingBox3d -> BoundingBox3d -> Bool
isContainedIn other boundingBox =
    (minX other <= minX boundingBox && maxX boundingBox <= maxX other)
        && (minY other <= minY boundingBox && maxY boundingBox <= maxY other)
        && (minZ other <= minZ boundingBox && maxZ boundingBox <= maxZ other)


{-| -}
hull : BoundingBox3d -> BoundingBox3d -> BoundingBox3d
hull firstBox secondBox =
    fromExtrema
        { minX = min (minX firstBox) (minX secondBox)
        , maxX = max (maxX firstBox) (maxX secondBox)
        , minY = min (minY firstBox) (minY secondBox)
        , maxY = max (maxY firstBox) (maxY secondBox)
        , minZ = min (minZ firstBox) (minZ secondBox)
        , maxZ = max (maxZ firstBox) (maxZ secondBox)
        }


{-| -}
intersection : BoundingBox3d -> BoundingBox3d -> Maybe BoundingBox3d
intersection firstBox secondBox =
    if intersects firstBox secondBox then
        Just
            (fromExtrema
                { minX = max (minX firstBox) (minX secondBox)
                , maxX = min (maxX firstBox) (maxX secondBox)
                , minY = max (minY firstBox) (minY secondBox)
                , maxY = min (maxY firstBox) (maxY secondBox)
                , minZ = max (minZ firstBox) (minZ secondBox)
                , maxZ = min (maxZ firstBox) (maxZ secondBox)
                }
            )
    else
        Nothing


{-| -}
scaleAbout : Point3d -> Float -> BoundingBox3d -> BoundingBox3d
scaleAbout point scale boundingBox =
    let
        ( x0, y0, z0 ) =
            Point3d.coordinates point
    in
    if scale >= 0 then
        fromExtrema
            { minX = x0 + scale * (minX boundingBox - x0)
            , maxX = x0 + scale * (maxX boundingBox - x0)
            , minY = y0 + scale * (minY boundingBox - y0)
            , maxY = y0 + scale * (maxY boundingBox - y0)
            , minZ = z0 + scale * (minZ boundingBox - z0)
            , maxZ = z0 + scale * (maxZ boundingBox - z0)
            }
    else
        fromExtrema
            { minX = x0 + scale * (maxX boundingBox - x0)
            , maxX = x0 + scale * (minX boundingBox - x0)
            , minY = y0 + scale * (maxY boundingBox - y0)
            , maxY = y0 + scale * (minY boundingBox - y0)
            , minZ = z0 + scale * (maxZ boundingBox - z0)
            , maxZ = z0 + scale * (minZ boundingBox - z0)
            }


{-| -}
translateBy : Vector3d -> BoundingBox3d -> BoundingBox3d
translateBy displacement boundingBox =
    let
        ( dx, dy, dz ) =
            Vector3d.components displacement
    in
    fromExtrema
        { minX = minX boundingBox + dx
        , maxX = maxX boundingBox + dx
        , minY = minY boundingBox + dy
        , maxY = maxY boundingBox + dy
        , minZ = minZ boundingBox + dz
        , maxZ = maxZ boundingBox + dz
        }


{-| -}
translateIn : Direction3d -> Float -> BoundingBox3d -> BoundingBox3d
translateIn direction distance boundingBox =
    translateBy (Vector3d.withLength distance direction) boundingBox
