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


{-| Construct a bounding box from its minimum and maximum X and Y values:

    exampleBox =
        BoundingBox2d.fromExtrema
            { minX = 3
            , maxX = 8
            , minY = 2
            , maxY = 6
            }

If the minimum and maximum values are provided in the wrong order (for example
if <code>minX&nbsp;>&nbsp;maxX</code>), then they will be swapped so that the
resulting bounding box is valid.

-}
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


{-| Construct a zero-width bounding box containing a single point.

    point =
        Point2d.fromCoordinates ( 2, 3 )

    BoundingBox2d.singleton point
    --> BoundingBox2d.fromExtrema
    -->     { minX = 2
    -->     , maxX = 2
    -->     , minY = 3
    -->     , maxY = 3
    -->     }

-}
singleton : Point2d -> BoundingBox2d
singleton point =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
    fromExtrema { minX = x, maxX = x, minY = y, maxY = y }


{-| Construct a bounding box with the two given points as two of its corners.
The points can be given in any order and don't have to represent the 'primary'
diagonal of the bounding box.

    firstPoint =
        Point2d.fromCoordinates ( 2, 3 )

    secondPoint =
        Point2d.fromCoordinates ( -1, 5 )

    BoundingBox2d.from firstPoint secondPoint
    --> BoundingBox2d.fromExtrema
    -->     { minX = -1
    -->     , maxX = 2
    -->     , minY = 3
    -->     , maxY = 5
    -->     }

-}
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


{-| Construct a bounding box containing all bounding boxes in the given list. If
the list is empty, returns `Nothing`.

    singletonBox =
        BoundingBox2d.singleton
            (Point2d.fromCoordinates ( 1, 3 ))

    BoundingBox2d.aggregate [ exampleBox, singletonBox ]
    --> Just
    -->     (BoundingBox2d.fromExtrema
    -->         { minX = 1,
    -->         , maxX = 8
    -->         , minY = 2
    -->         , maxY = 6
    -->         }
    -->     )

    BoundingBox2d.aggregate [ exampleBox ]
    --> Just exampleBox

    BoundingBox2d.aggregate []
    --> Nothing

If you have exactly two bounding boxes, you can use [`BoundingBox2d.hull`](#hull)
instead (which returns a `BoundingBox2d` instead of a `Maybe BoundingBox2d`).

-}
aggregate : List BoundingBox2d -> Maybe BoundingBox2d
aggregate boundingBoxes =
    case boundingBoxes of
        first :: rest ->
            Just (List.foldl hull first rest)

        [] ->
            Nothing


{-| Construct a bounding box containing all points in the given list. If the
list is empty, returns `Nothing`.

    BoundingBox2d.containingPoints
        [ Point2d.fromCoordinates ( 2, 3 )
        , Point2d.fromCoordinates ( -1, 5 )
        , Point2d.fromCoordinates ( 6, 4 )
        ]
    --> Just <|
    -->     BoundingBox2d.fromExtrema
    -->         { minX = -1
    -->         , maxX = 6
    -->         , minY = 3
    -->         , maxY = 5
    -->         }

    BoundingBox2d.containingPoints []
    --> Nothing

-}
containingPoints : List Point2d -> Maybe BoundingBox2d
containingPoints points =
    aggregate (List.map singleton points)


{-| Get the minimum and maximum X and Y values of a bounding box in a single
record.

    BoundingBox2d.extrema exampleBox
    --> { minX = 3
    --> , maxX = 8
    --> , minY = 2
    --> , maxY = 6
    --> }

Can be useful when combined with record destructuring, for example

    { minX, maxX, minY, maxY } =
        BoundingBox2d.extrema exampleBox


    --> minX = 3
    --> maxX = 8
    --> minY = 2
    --> maxY = 6

-}
extrema : BoundingBox2d -> { minX : Float, maxX : Float, minY : Float, maxY : Float }
extrema (Types.BoundingBox2d extrema_) =
    extrema_


{-| Get the minimum X value of a bounding box.

    BoundingBox2d.minX exampleBox
    --> 3

-}
minX : BoundingBox2d -> Float
minX (Types.BoundingBox2d boundingBox) =
    boundingBox.minX


{-| Get the maximum X value of a bounding box.

    BoundingBox2d.maxX exampleBox
    --> 8

-}
maxX : BoundingBox2d -> Float
maxX (Types.BoundingBox2d boundingBox) =
    boundingBox.maxX


{-| Get the minimum Y value of a bounding box.

    BoundingBox2d.minY exampleBox
    --> 2

-}
minY : BoundingBox2d -> Float
minY (Types.BoundingBox2d boundingBox) =
    boundingBox.minY


{-| Get the maximum Y value of a bounding box.

    BoundingBox2d.maxY exampleBox
    --> 6

-}
maxY : BoundingBox2d -> Float
maxY (Types.BoundingBox2d boundingBox) =
    boundingBox.maxY


{-| Get the X and Y dimensions (width and height) of a bounding box.

    ( width, height ) =
        BoundingBox2d.dimensions exampleBox


    --> width = 5
    --> height = 4

-}
dimensions : BoundingBox2d -> ( Float, Float )
dimensions boundingBox =
    ( maxX boundingBox - minX boundingBox
    , maxY boundingBox - minY boundingBox
    )


{-| Get the median X value of a bounding box.

    BoundingBox2d.midX exampleBox
    --> 5.5

-}
midX : BoundingBox2d -> Float
midX (Types.BoundingBox2d boundingBox) =
    boundingBox.minX + 0.5 * (boundingBox.maxX - boundingBox.minX)


{-| Get the median Y value of a bounding box.

    BoundingBox2d.midY exampleBox
    --> 4

-}
midY : BoundingBox2d -> Float
midY (Types.BoundingBox2d boundingBox) =
    boundingBox.minY + 0.5 * (boundingBox.maxY - boundingBox.minY)


{-| Get the point at the center of a bounding box.

    BoundingBox2d.centroid exampleBox
    --> Point2d.fromCoordinates ( 5.5, 4 )

-}
centroid : BoundingBox2d -> Point2d
centroid boundingBox =
    Point2d.fromCoordinates ( midX boundingBox, midY boundingBox )


{-| Check if a bounding box contains a particular point.

    point =
        Point2d.fromCoordinates ( 4, 3 )

    BoundingBox2d.contains point exampleBox
    --> True

    BoundingBox2d.contains Point2d.origin exampleBox
    --> False

-}
contains : Point2d -> BoundingBox2d -> Bool
contains point boundingBox =
    let
        ( x, y ) =
            Point2d.coordinates point
    in
    (minX boundingBox <= x && x <= maxX boundingBox)
        && (minY boundingBox <= y && y <= maxY boundingBox)


{-| Test if two boxes touch or overlap at all (have any points in common);

    BoundingBox2d.intersects firstBox secondBox

is equivalent to

    BoundingBox2d.intersection firstBox secondBox
        /= Nothing

but is more efficient.

    firstBox =
        BoundingBox2d.fromExtrema
            { minX = 0
            , maxX = 3
            , minY = 0
            , maxY = 2
            }

    secondBox =
        BoundingBox2d.fromExtrema
            { minX = 0
            , maxX = 3
            , minY = 1
            , maxY = 4
            }

    thirdBox =
        BoundingBox2d.fromExtrema
            { minX = 0
            , maxX = 3
            , minY = 4
            , maxY = 5
            }

    BoundingBox2d.intersects firstBox secondBox
    --> True

    BoundingBox2d.intersects firstBox thirdBox
    --> False

-}
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


{-| Check if one box overlaps another by less than, greater than or equal to a
given amount. For example, you could implement a tolerant collision check (one
that only returns true if the boxes overlap by at least some small finite
amount, and ignores boxes that just barely touch each other) as

    boxesCollide box1 box2 =
        BoundingBox2d.overlappingBy GT 0.001 box1 box2

This can be read as "`box1` and `box2` are overlapping by greater than 0.001
units". (The [`Order`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#Order)
type and its three values `LT`, `GT` and `EQ` are defined in Elm's `Basics`
module so are available by default in any Elm program.)

Overlap is defined as the _minimum_ distance one box would have to move so that
it did not touch the other, and is always positive for any two overlapping
boxes.

Boxes that just touch are considered to have an overlap of zero, which is
distinct from 'no overlap'. Boxes that do not touch or overlap at all are
considered to have an overlap which is less than zero but not comparable to any
negative number.


### Less than

  - `overlappingBy LT 1e-3` will return true if the two boxes overlap by less
    than 0.001 units or if they do not overlap at all (false if they overlap by
    more than 0.001 units).
  - `overlappingBy LT 0` will return true only if the two boxes don't touch or
    overlap at all.
  - `overlappingBy LT -1e-3` will always return false! If you care about _how
    much_ two boxes are separated by, use `separatedBy` instead.


### Greater than

  - `overlappingBy GT 1e-3` will return true if the two boxes overlap by at
    least 0.001 units (false if they overlap by less than that or do not overlap
    at all).
  - `overlappingBy GT 0` will return true if the two boxes overlap by any
    non-zero amount (false if they just touch or do not overlap at all).
  - `overlappingBy GT -1e-3` doesn't make a lot of sense but will return true if
    the boxes touch or overlap at all (false if they don't overlap, regardless
    of how close they are to overlapping). In this case, though, it would make
    more sense to just user `intersects` instead.


### Equal to

Checking whether two boxes overlap by exactly a given amount is pretty weird and
vulnerable to floating-point roundoff, but is defined as follows:

  - `overlappingBy EQ 1e-3` will return true if the two boxes overlap by exactly
    0.001 units.
  - `overlappingBy EQ 0` will return true if and only if the boxes just touch
    each other.
  - `overlappingBy EQ -1e-3` will always return false.

-}
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


{-| Check if one box is separated from another by less than, greater than or
equal to a given amount. For example, to perform clash detection between some
objects, you could use `separatedBy` on those objects' bounding boxes as a quick
check to see if the objects had a gap of at least 1 cm between them:

    safelySeparated box1 box2 =
        BoundingBox2d.separatedBy GT 0.01 box1 box2

This can be read as "`box1` and `box2` are separated by greater than 0.01
units". (The [`Order`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#Order)
type and its three values `LT`, `GT` and `EQ` are defined in Elm's `Basics`
module so are available by default in any Elm program.)

Separation is defined as the _minimum_ distance one box would have to move
so that it touched the other, and is always positive for any two boxes that do
not touch.

Boxes that just touch are considered to have a separation of zero, which is
distinct from 'no separation'. 'No separation' (overlap) is considered to be
less than zero but not comparable to any negative number.


### Less than

  - `separatedBy LT 1e-3` will return true if the two boxes are separated by
    less than 0.001 units or if they touch or overlap (false if they are
    separated by at least 0.001 units).
  - `separatedBy LT 0` will return true only if the boxes overlap by some
    non-zero amount.
  - `separatedBy LT -1e-3` will always return false! If you care about _how
    much_ two boxes overlap by, use `overlappingBy` instead.


### Greater than

  - `separatedBy GT 1e-3` will return true if the two boxes are separated by at
    least 0.001 units (false if they are separated by less than that or if they
    touch or overlap).
  - `separatedBy GT 0` will return true if the two boxes are separated by any
    non-zero amount (false if they touch or overlap).
  - `separatedBy GT -1e-3` doesn't make a lot of sense but will return true if
    the boxes just touch or are separated by any amount (false if they overlap
    by any non-zero amount).


### Equal to

Checking whether two boxes are separated by exactly a given amount is pretty
weird and vulnerable to floating-point roundoff, but is defined as follows:

  - `separatedBy EQ 1e-3` will return true if the two boxes are separated by
    exactly 0.001 units.
  - `separatedBy EQ 0` will return true if and only if the boxes just touch each
    other.
  - `separatedBy EQ -3` will always return false.

-}
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


{-| Test if the second given bounding box is fully contained within the first
(is a subset of it).

    outerBox =
        BoundingBox2d.fromExtrema
            { minX = 0
            , maxX = 10
            , minY = 0
            , maxY = 10
            }

    innerBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 5
            , minY = 3
            , maxY = 9
            }

    overlappingBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 5
            , minY = 3
            , maxY = 12
            }

    BoundingBox2d.isContainedIn outerBox innerBox
    --> True

    BoundingBox2d.isContainedIn outerBox overlappingBox
    --> False

-}
isContainedIn : BoundingBox2d -> BoundingBox2d -> Bool
isContainedIn other boundingBox =
    (minX other <= minX boundingBox && maxX boundingBox <= maxX other)
        && (minY other <= minY boundingBox && maxY boundingBox <= maxY other)


{-| Build a bounding box that contains both given bounding boxes.

    firstBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            }

    secondBox =
        BoundingBox2d.fromExtrema
            { minX = -2
            , maxX = 2
            , minY = 4
            , maxY = 5
            }

    BoundingBox2d.hull firstBox secondBox
    --> BoundingBox2d.fromExtrema
    -->     { minX = -2
    -->     , maxX = 4
    -->     , minY = 2
    -->     , maxY = 5
    -->     }

-}
hull : BoundingBox2d -> BoundingBox2d -> BoundingBox2d
hull firstBox secondBox =
    fromExtrema
        { minX = min (minX firstBox) (minX secondBox)
        , maxX = max (maxX firstBox) (maxX secondBox)
        , minY = min (minY firstBox) (minY secondBox)
        , maxY = max (maxY firstBox) (maxY secondBox)
        }


{-| Attempt to build a bounding box that contains all points common to both
given bounding boxes. If the given boxes do not intersect, returns `Nothing`.

    firstBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 4
            , minY = 2
            , maxY = 3
            }

    secondBox =
        BoundingBox2d.fromExtrema
            { minX = 2
            , maxX = 5
            , minY = 1
            , maxY = 4
            }

    thirdBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 4
            , minY = 4
            , maxY = 5
            }

    BoundingBox2d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox2d.fromExtrema
    -->         { minX = 2
    -->         , maxX = 4
    -->         , minY = 2
    -->         , maxY = 3
    -->         }
    -->     )

    BoundingBox2d.intersection firstBox thirdBox
    --> Nothing

If two boxes just touch along an edge or at a corner, they are still considered
to have an intersection, even though that intersection will have zero area (at
least one of its dimensions will be zero):

    firstBox =
        BoundingBox2d.fromExtrema
            { minX = 0
            , maxX = 1
            , minY = 0
            , maxY = 2
            }

    secondBox =
        BoundingBox2d.fromExtrema
            { minX = 1
            , maxX = 2
            , minY = 1
            , maxY = 3
            }

    BoundingBox2d.intersection firstBox secondBox
    --> Just
    -->     (BoundingBox2d.fromExtrema
    -->         { minX = 1
    -->         , maxX = 1
    -->         , minY = 1
    -->         , maxY = 2
    -->         }
    -->     )

-}
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


{-| Scale a bounding box about a given point by a given scale.

    point =
        Point2d.fromCoordinates ( 4, 4 )

    BoundingBox2d.scaleAbout point 2 exampleBox
    --> BoundingBox2d.fromExtrema
    -->     { minX = 2
    -->     , maxX = 12
    -->     , minY = 0
    -->     , maxY = 8
    -->     }

-}
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


{-| Translate a bounding box by a given displacement.

    displacement =
        Vector2d.fromComponents ( 2, -3 )

    BoundingBox2d.translateBy displacement exampleBox
    --> BoundingBox2d.fromExtrema
    -->     { minX = 5
    -->     , maxX = 10
    -->     , minY = -1
    -->     , maxY = 3
    -->     }

-}
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


{-| Translate a bounding box in a given direction by a given distance;

    BoundingBox2d.translateIn direction distance

is equivalent to

    BoundingBox2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> BoundingBox2d -> BoundingBox2d
translateIn direction distance boundingBox =
    translateBy (Vector2d.withLength distance direction) boundingBox
