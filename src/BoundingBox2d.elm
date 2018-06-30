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


module BoundingBox2d
    exposing
        ( BoundingBox2d
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
        , midX
        , midY
        , minX
        , minY
        , overlappingBy
        , scaleAbout
        , separatedBy
        , singleton
        , translateBy
        , translateIn
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/BoundingBox2d/icon.svg" alt="BoundingBox2d" width="160">

A `BoundingBox2d` is a rectangular box in 2D defined by its minimum and maximum
X and Y values. It is possible to generate bounding boxes for most geometric
objects; for example, [`Triangle2d.boundingBox`](Triangle2d#boundingBox) takes a
`Triangle2d` and returns a `BoundingBox2d` that contains that triangle. There
are several use cases where it is more efficient to deal with the bounding box
of an object than the object itself, such as:

  - Intersection checking: If (for example) the bounding boxes of a line segment
    and a triangle do not overlap, then the line segment and triangle cannot
    possibly intersect each other. Expensive intersection checking therefore
    only has to be performed for line segments and triangles whose bounding
    boxes _do_ overlap.
  - 2D rendering: When rendering a 2D scene, any object whose bounding box does
    not overlap the viewing area must itself be completely outside the viewing
    area, and therefore does not have to be drawn. This provides a simple form
    of culling.

@docs BoundingBox2d


# Constructors

@docs fromExtrema, singleton, from, hull, intersection, aggregate, containingPoints


# Properties

@docs extrema, minX, maxX, minY, maxY, dimensions, midX, midY, centroid


# Queries

@docs contains, isContainedIn, intersects, overlappingBy, separatedBy


# Transformations

@docs scaleAbout, translateBy, translateIn

-}

import Direction2d exposing (Direction2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias BoundingBox2d =
    Types.BoundingBox2d


{-| -}
fromExtrema : { minX : Float, maxX : Float, minY : Float, maxY : Float } -> BoundingBox2d
fromExtrema extrema_ =
    if extrema_.minX <= extrema_.maxX && extrema_.minY <= extrema_.maxY then
        Types.BoundingBox2d extrema_
    else
        Types.BoundingBox2d
            { minX = min extrema_.minX extrema_.maxX
            , maxX = max extrema_.minX extrema_.maxX
            , minY = min extrema_.minY extrema_.maxY
            , maxY = max extrema_.minY extrema_.maxY
            }


{-| -}
singleton : Point2d -> BoundingBox2d
singleton point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
    fromExtrema { minX = x, maxX = x, minY = y, maxY = y }


{-| -}
from : Point2d -> Point2d -> BoundingBox2d
from firstPoint secondPoint =
    let
        ( x1, y1 ) =
            Point2d.coordinates firstPoint

        ( x2, y2 ) =
            Point2d.coordinates secondPoint
    in
    fromExtrema
        { minX = min x1 x2
        , maxX = max x1 x2
        , minY = min y1 y2
        , maxY = max y1 y2
        }


{-| -}
aggregate : List BoundingBox2d -> Maybe BoundingBox2d
aggregate boundingBoxes =
    case boundingBoxes of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| -}
containingPoints : List Point2d -> Maybe BoundingBox2d
containingPoints points =
    aggregate (List.map singleton points)


{-| -}
extrema : BoundingBox2d -> { minX : Float, maxX : Float, minY : Float, maxY : Float }
extrema (Types.BoundingBox2d extrema_) =
    extrema_


{-| -}
minX : BoundingBox2d -> Float
minX (Types.BoundingBox2d boundingBox) =
    boundingBox.minX


{-| -}
maxX : BoundingBox2d -> Float
maxX (Types.BoundingBox2d boundingBox) =
    boundingBox.maxX


{-| -}
minY : BoundingBox2d -> Float
minY (Types.BoundingBox2d boundingBox) =
    boundingBox.minY


{-| -}
maxY : BoundingBox2d -> Float
maxY (Types.BoundingBox2d boundingBox) =
    boundingBox.maxY


{-| -}
dimensions : BoundingBox2d -> ( Float, Float )
dimensions boundingBox =
    ( maxX boundingBox - minX boundingBox
    , maxY boundingBox - minY boundingBox
    )


{-| -}
midX : BoundingBox2d -> Float
midX (Types.BoundingBox2d boundingBox) =
    boundingBox.minX + 0.5 * (boundingBox.maxX - boundingBox.minX)


{-| -}
midY : BoundingBox2d -> Float
midY (Types.BoundingBox2d boundingBox) =
    boundingBox.minY + 0.5 * (boundingBox.maxY - boundingBox.minY)


{-| -}
centroid : BoundingBox2d -> Point2d
centroid boundingBox =
    Point2d.fromCoordinates ( midX boundingBox, midY boundingBox )


{-| -}
contains : Point2d -> BoundingBox2d -> Bool
contains point boundingBox =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
    (minX boundingBox <= x && x <= maxX boundingBox)
        && (minY boundingBox <= y && y <= maxY boundingBox)


{-| -}
intersects : BoundingBox2d -> BoundingBox2d -> Bool
intersects other boundingBox =
    (minX boundingBox <= maxX other)
        && (maxX boundingBox >= minX other)
        && (minY boundingBox <= maxY other)
        && (maxY boundingBox >= minY other)


overlapAmount : BoundingBox2d -> BoundingBox2d -> Maybe Float
overlapAmount firstBox secondBox =
    let
        xOverlap =
            min (maxX firstBox) (maxX secondBox)
                - max (minX firstBox) (minX secondBox)

        yOverlap =
            min (maxY firstBox) (maxY secondBox)
                - max (minY firstBox) (minY secondBox)
    in
    if xOverlap >= 0 && yOverlap >= 0 then
        Just (min xOverlap yOverlap)
    else
        Nothing


squaredSeparationAmount : BoundingBox2d -> BoundingBox2d -> Maybe Float
squaredSeparationAmount firstBox secondBox =
    let
        xSeparation =
            max (minX firstBox) (minX secondBox)
                - min (maxX firstBox) (maxX secondBox)

        ySeparation =
            max (minY firstBox) (minY secondBox)
                - min (maxY firstBox) (maxY secondBox)
    in
    if xSeparation > 0 && ySeparation > 0 then
        Just (xSeparation * xSeparation + ySeparation * ySeparation)
    else if xSeparation > 0 then
        Just (xSeparation * xSeparation)
    else if ySeparation > 0 then
        Just (ySeparation * ySeparation)
    else if xSeparation == 0 || ySeparation == 0 then
        Just 0
    else
        Nothing


alwaysFalse : BoundingBox2d -> BoundingBox2d -> Bool
alwaysFalse firstBox secondBox =
    False


{-| -}
overlappingBy : Order -> Float -> BoundingBox2d -> BoundingBox2d -> Bool
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
separatedBy : Order -> Float -> BoundingBox2d -> BoundingBox2d -> Bool
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
isContainedIn : BoundingBox2d -> BoundingBox2d -> Bool
isContainedIn other boundingBox =
    (minX other <= minX boundingBox && maxX boundingBox <= maxX other)
        && (minY other <= minY boundingBox && maxY boundingBox <= maxY other)


{-| -}
hull : BoundingBox2d -> BoundingBox2d -> BoundingBox2d
hull firstBox secondBox =
    fromExtrema
        { minX = min (minX firstBox) (minX secondBox)
        , maxX = max (maxX firstBox) (maxX secondBox)
        , minY = min (minY firstBox) (minY secondBox)
        , maxY = max (maxY firstBox) (maxY secondBox)
        }


{-| -}
intersection : BoundingBox2d -> BoundingBox2d -> Maybe BoundingBox2d
intersection firstBox secondBox =
    if intersects firstBox secondBox then
        Just
            (fromExtrema
                { minX = max (minX firstBox) (minX secondBox)
                , maxX = min (maxX firstBox) (maxX secondBox)
                , minY = max (minY firstBox) (minY secondBox)
                , maxY = min (maxY firstBox) (maxY secondBox)
                }
            )
    else
        Nothing


{-| -}
scaleAbout : Point2d -> Float -> BoundingBox2d -> BoundingBox2d
scaleAbout point scale boundingBox =
    let
        ( x0, y0 ) =
            Point2d.coordinates point
    in
    if scale >= 0 then
        fromExtrema
            { minX = x0 + scale * (minX boundingBox - x0)
            , maxX = x0 + scale * (maxX boundingBox - x0)
            , minY = y0 + scale * (minY boundingBox - y0)
            , maxY = y0 + scale * (maxY boundingBox - y0)
            }
    else
        fromExtrema
            { minX = x0 + scale * (maxX boundingBox - x0)
            , maxX = x0 + scale * (minX boundingBox - x0)
            , minY = y0 + scale * (maxY boundingBox - y0)
            , maxY = y0 + scale * (minY boundingBox - y0)
            }


{-| -}
translateBy : Vector2d -> BoundingBox2d -> BoundingBox2d
translateBy displacement boundingBox =
    let
        ( dx, dy ) =
            Vector2d.components displacement
    in
    fromExtrema
        { minX = minX boundingBox + dx
        , maxX = maxX boundingBox + dx
        , minY = minY boundingBox + dy
        , maxY = maxY boundingBox + dy
        }


{-| -}
translateIn : Direction2d -> Float -> BoundingBox2d -> BoundingBox2d
translateIn direction distance boundingBox =
    translateBy (Vector2d.withLength distance direction) boundingBox
