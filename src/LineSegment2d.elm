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


module LineSegment2d
    exposing
        ( LineSegment2d
        , along
        , boundingBox
        , direction
        , endPoint
        , endpoints
        , from
        , fromEndpoints
        , interpolate
        , intersectionPoint
        , length
        , mapEndpoints
        , midpoint
        , mirrorAcross
        , perpendicularDirection
        , placeIn
        , projectOnto
        , relativeTo
        , reverse
        , rotateAround
        , scaleAbout
        , squaredLength
        , startPoint
        , translateBy
        , translateIn
        , vector
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/LineSegment2d/icon.svg" alt="LineSegment2d" width="160">

A `LineSegment2d` is a line between two points in 2D. This module contains
functionality such as:

  - Interpolating within a line segment or measuring its length
  - Scaling, rotating, translating, mirroring or projecting a line segment
  - Converting a line segment between local and global coordinates in different
    reference frames

@docs LineSegment2d


# Constructors

@docs fromEndpoints, from, along


# Properties

@docs startPoint, endPoint, endpoints, midpoint, length, squaredLength, direction, perpendicularDirection, vector, boundingBox


# Interpolation

@docs interpolate


# Intersection

@docs intersectionPoint


# Transformations

Transforming a line segment is equivalent to transforming its start and end
points and forming a new line segment between the resulting points.

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapEndpoints


# Coordinate conversions

Functions for transforming line segments between local and global coordinates in
different coordinate frames.

@docs relativeTo, placeIn

-}

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias LineSegment2d =
    Types.LineSegment2d


{-| Construct a line segment from its two endpoints:

    exampleLineSegment =
        LineSegment2d.fromEndpoints
            ( Point2d.fromCoordinates ( 1, 2 )
            , Point2d.fromCoordinates ( 3, 4 )
            )

-}
fromEndpoints : ( Point2d, Point2d ) -> LineSegment2d
fromEndpoints =
    Types.LineSegment2d


{-| Construct a line segment from the first point to the second;

    LineSegment2d.from firstPoint secondPoint

is equivalent to

    LineSegment2d.fromEndpoints ( firstPoint, secondPoint )

-}
from : Point2d -> Point2d -> LineSegment2d
from startPoint_ endPoint_ =
    fromEndpoints ( startPoint_, endPoint_ )


{-| Construct a line segment lying on the given axis, with its endpoints at the
given distances from the axis' origin point.

    LineSegment2d.along Axis2d.x 3 5
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 3, 0 )
    -->     , Point2d.fromCoordinates ( 5, 0 )
    -->     )

    LineSegment2d.along Axis2d.y 2 -4
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 0, 2 )
    -->     , Point2d.fromCoordinates ( 0, -4 )
    -->     )

-}
along : Axis2d -> Float -> Float -> LineSegment2d
along axis start end =
    fromEndpoints ( Point2d.along axis start, Point2d.along axis end )


{-| Get the start point of a line segment.

    LineSegment2d.startPoint exampleLineSegment
    --> Point2d.fromCoordinates ( 1, 2 )

-}
startPoint : LineSegment2d -> Point2d
startPoint (Types.LineSegment2d ( start, _ )) =
    start


{-| Get the end point of a line segment.

    LineSegment2d.endPoint exampleLineSegment
    --> Point2d.fromCoordinates ( 3, 4 )

-}
endPoint : LineSegment2d -> Point2d
endPoint (Types.LineSegment2d ( _, end )) =
    end


{-| Get the endpoints of a line segment as a tuple.

    ( p1, p2 ) =
        LineSegment2d.endpoints lineSegment

-}
endpoints : LineSegment2d -> ( Point2d, Point2d )
endpoints (Types.LineSegment2d endpoints_) =
    endpoints_


{-| Reverse a line segment, swapping its start and end points.

    LineSegment2d.reverse exampleLineSegment
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 3, 4 )
    -->     , Point2d.fromCoordinates ( 1, 2 )
    -->     )

-}
reverse : LineSegment2d -> LineSegment2d
reverse lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( p2, p1 )


{-| Get the midpoint of a line segment.

    LineSegment2d.midpoint exampleLineSegment
    --> Point2d.fromCoordinates ( 2, 3 )

-}
midpoint : LineSegment2d -> Point2d
midpoint lineSegment =
    interpolate lineSegment 0.5


{-| Interpolate a line segment between its start and end points; a value of 0.0
corresponds to the start point of the line segment, a value of 0.5 corresponds
to its midpoint and a value of 1.0 corresponds to its end point. Values less
than 0.0 or greater than 1.0 can be used to extrapolate.

    LineSegment2d.interpolate exampleLineSegment 0.25
    --> Point2d.fromCoordinates ( 1.5, 2.5 )

    LineSegment2d.interpolate exampleLineSegment 1.5
    --> Point2d.fromCoordinates ( 4, 5 )

-}
interpolate : LineSegment2d -> Float -> Point2d
interpolate lineSegment =
    let
        ( start, end ) =
            endpoints lineSegment
    in
    Point2d.interpolateFrom start end


{-| Get the length of a line segment.

    LineSegment2d.length exampleLineSegment
    --> 2.8284

-}
length : LineSegment2d -> Float
length =
    vector >> Vector2d.length


{-| Get the squared length of a line segment. Slightly more efficient than
`length` since it avoids a square root.

    LineSegment2d.squaredLength exampleLineSegment
    --> 8

-}
squaredLength : LineSegment2d -> Float
squaredLength =
    vector >> Vector2d.squaredLength


{-| Get the direction from a line segment's start point to its end point. If the
line segment has zero length (the start and end points are the same), returns
`Nothing`.

    LineSegment2d.direction exampleLineSegment
    --> Just (Direction2d.fromAngle (degrees 45))

-}
direction : LineSegment2d -> Maybe Direction2d
direction =
    vector >> Vector2d.direction


{-| Get the direction perpendicular to a line segment, pointing to the left. If
the line segment has zero length, returns `Nothing`.

    LineSegment2d.perpendicularDirection exampleLineSegment
    --> Just (Direction2d.fromAngle (degrees 135))

-}
perpendicularDirection : LineSegment2d -> Maybe Direction2d
perpendicularDirection =
    vector >> Vector2d.perpendicularTo >> Vector2d.direction


{-| Get the vector from a given line segment's start point to its end point.

    LineSegment2d.vector exampleLineSegment
    --> Vector2d.fromComponents ( 2, 2 )

-}
vector : LineSegment2d -> Vector2d
vector lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    Vector2d.from p1 p2


{-| Attempt to find the unique intersection point of two line segments. If there
is no such point (the two line segments do not touch, or they overlap), returns
`Nothing`.

    -- 4 corners of a square

    a =
        Point2d.fromCoordinates ( 0, 0 )

    b =
        Point2d.fromCoordinates ( 1, 0 )

    c =
        Point2d.fromCoordinates ( 1, 1 )

    d =
        Point2d.fromCoordinates ( 0, 1 )

    -- definition of some segments with those points

    ab =
        LineSegment2d.from a b
    ...

    -- searching for intersections

    LineSegment2d.intersectionPoint ab bc
    --> Just (Point2d.fromCoordinates ( 1, 0 ))
    -- corner point b

    LineSegment2d.intersectionPoint ac bd
    --> Just (Point2d.fromCoordinates ( 0.5, 0.5 ))
    -- diagonal crossing at square center

    LineSegment2d.intersectionPoint ab cd
    --> Nothing -- parallel lines

    LineSegment2d.intersectionPoint ab ab
    --> Nothing -- collinear lines

Note that if the endpoint of one line segment lies on the other line segment,
numerical roundoff means that the intersection may or may not be found. If two
segments have a shared endpoint (the two segments meet in something like a 'V',
where the end point of one segment is the start point of the next), that point
is guaranteed to be returned as the intersection point, but if two segments meet
in a 'T' shape the intersection point may or may not be found.

-}
intersectionPoint : LineSegment2d -> LineSegment2d -> Maybe Point2d
intersectionPoint lineSegment1 lineSegment2 =
    -- The two line segments are:
    -- p |--- r ---| p_
    -- q |--- s ---| q_
    let
        ( p, p_ ) =
            endpoints lineSegment1

        ( q, q_ ) =
            endpoints lineSegment2

        r =
            vector lineSegment1

        s =
            vector lineSegment2

        pq =
            Vector2d.from p q

        pq_ =
            Vector2d.from p q_

        qp_ =
            Vector2d.from q p_

        pqXr =
            Vector2d.crossProduct pq r

        pqXs =
            Vector2d.crossProduct pq s

        sXqp_ =
            Vector2d.crossProduct s qp_

        rXpq_ =
            Vector2d.crossProduct r pq_

        tDenominator =
            pqXs - sXqp_

        uDenominator =
            pqXr + rXpq_
    in
    if tDenominator == 0 || uDenominator == 0 then
        -- Segments are parallel or collinear.
        -- In collinear case, we check if there is only one intersection point.
        if Vector2d.dotProduct r s < 0 then
            if p_ == q_ then
                -- p |----- p_ | q_ -----| q
                Just p_
            else if p == q then
                -- q_ |----- q | p -----| p_
                Just p
            else
                Nothing
        else if p_ == q then
            -- p |----- p_ | q -----| q_
            Just p_
        else if p == q_ then
            -- q |----- q_ | p -----| p_
            Just p
        else
            Nothing
    else
        -- Segments are not parallel.
        -- We search for the intersection point of the two lines.
        let
            t =
                pqXs / tDenominator

            u =
                pqXr / uDenominator
        in
        if (0 <= t && t <= 1) && (0 <= u && u <= 1) then
            -- Intersection is within both segments.
            let
                -- Ensure interpolation happens from the closest
                -- endpoint (this should be more numerically stable, and
                -- also mostly ensures that intersection is symmetric)
                intersection =
                    if min t (1 - t) <= min u (1 - u) then
                        interpolate lineSegment1 t
                    else
                        interpolate lineSegment2 u
            in
            Just intersection
        else
            Nothing


{-| Scale a line segment about the given center point by the given scale.

    point =
        Point2d.fromCoordinates ( 1, 1 )

    LineSegment2d.scaleAbout point 2 exampleLineSegment
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 1, 3 )
    -->     , Point2d.fromCoordinates ( 5, 7 )
    -->     )

-}
scaleAbout : Point2d -> Float -> LineSegment2d -> LineSegment2d
scaleAbout point scale =
    mapEndpoints (Point2d.scaleAbout point scale)


{-| Rotate a line segment counterclockwise around a given center point by a
given angle (in radians).

    exampleLineSegment
        |> LineSegment2d.rotateAround Point2d.origin
            (degrees 90)
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( -2, 1 )
    -->     , Point2d.fromCoordinates ( -4, 3 )
    -->     )

-}
rotateAround : Point2d -> Float -> LineSegment2d -> LineSegment2d
rotateAround centerPoint angle =
    mapEndpoints (Point2d.rotateAround centerPoint angle)


{-| Translate a line segment by a given displacement.

    displacement =
        Vector2d.fromComponents ( 1, 2 )

    exampleLineSegment
        |> LineSegment2d.translateBy displacement
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 2, 4 )
    -->     , Point2d.fromCoordinates ( 4, 6 )
    -->     )

-}
translateBy : Vector2d -> LineSegment2d -> LineSegment2d
translateBy displacementVector =
    mapEndpoints (Point2d.translateBy displacementVector)


{-| Translate a line segment in a given direction by a given distance;

    LineSegment2d.translateIn direction distance

is equivalent to

    LineSegment2d.translateBy
        (Vector2d.withLength distance direction)

-}
translateIn : Direction2d -> Float -> LineSegment2d -> LineSegment2d
translateIn translationDirection distance lineSegment =
    translateBy (Vector2d.withLength distance translationDirection) lineSegment


{-| Mirror a line segment across an axis.

    LineSegment2d.mirrorAcross Axis2d.y exampleLineSegment
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( -1, 2 )
    -->     , Point2d.fromCoordinates ( -3, 4 )
    -->     )

Note that the endpoints of a mirrored segment are equal to the mirrored
endpoints of the original segment, but as a result the normal direction of a
mirrored segment is the _opposite_ of the mirrored normal direction of the
original segment (since the normal direction is always considered to be 'to the
left' of the line segment).

-}
mirrorAcross : Axis2d -> LineSegment2d -> LineSegment2d
mirrorAcross axis =
    mapEndpoints (Point2d.mirrorAcross axis)


{-| Project a line segment onto an axis.

    LineSegment2d.projectOnto Axis2d.x exampleLineSegment
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 1, 0 )
    -->     , Point2d.fromCoordinates ( 3, 0 )
    -->     )

    LineSegment2d.projectOnto Axis2d.y exampleLineSegment
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 0, 2 )
    -->     , Point2d.fromCoordinates ( 0, 4 )
    -->     )

-}
projectOnto : Axis2d -> LineSegment2d -> LineSegment2d
projectOnto axis =
    mapEndpoints (Point2d.projectOnto axis)


{-| Transform the start and end points of a line segment by a given function
and create a new line segment from the resulting points. Most other
transformation functions can be defined in terms of `mapEndpoints`; for example,

    LineSegment2d.projectOnto axis

is equivalent to

    LineSegment2d.mapEndpoints (Point2d.projectOnto axis)

-}
mapEndpoints : (Point2d -> Point2d) -> LineSegment2d -> LineSegment2d
mapEndpoints function lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( function p1, function p2 )


{-| Take a line segment defined in global coordinates, and return it expressed
in local coordinates relative to a given reference frame.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    LineSegment2d.relativeTo localFrame exampleLineSegment
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 0, 0 )
    -->     , Point2d.fromCoordinates ( 2, 2 )
    -->     )

-}
relativeTo : Frame2d -> LineSegment2d -> LineSegment2d
relativeTo frame =
    mapEndpoints (Point2d.relativeTo frame)


{-| Take a line segment considered to be defined in local coordinates relative
to a given reference frame, and return that line segment expressed in global
coordinates.

    localFrame =
        Frame2d.atPoint (Point2d.fromCoordinates ( 1, 2 ))

    LineSegment2d.placeIn localFrame exampleLineSegment
    --> LineSegment2d.fromEndpoints
    -->     ( Point2d.fromCoordinates ( 2, 4 )
    -->     , Point2d.fromCoordinates ( 4, 6 )
    -->     )

-}
placeIn : Frame2d -> LineSegment2d -> LineSegment2d
placeIn frame =
    mapEndpoints (Point2d.placeIn frame)


{-| Get the minimal bounding box containing a given line segment.

    LineSegment2d.boundingBox exampleLineSegment
    --> BoundingBox2d.fromExtrema
    -->     { minX = 1
    -->     , maxX = 3
    -->     , minY = 2
    -->     , maxY = 4
    -->     }

-}
boundingBox : LineSegment2d -> BoundingBox2d
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    BoundingBox2d.from p1 p2
