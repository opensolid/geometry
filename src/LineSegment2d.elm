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

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapEndpoints


# Coordinate conversions

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


{-| -}
fromEndpoints : ( Point2d, Point2d ) -> LineSegment2d
fromEndpoints =
    Types.LineSegment2d


{-| -}
from : Point2d -> Point2d -> LineSegment2d
from startPoint_ endPoint_ =
    fromEndpoints ( startPoint_, endPoint_ )


{-| -}
along : Axis2d -> Float -> Float -> LineSegment2d
along axis start end =
    fromEndpoints ( Point2d.along axis start, Point2d.along axis end )


{-| -}
startPoint : LineSegment2d -> Point2d
startPoint (Types.LineSegment2d ( start, _ )) =
    start


{-| -}
endPoint : LineSegment2d -> Point2d
endPoint (Types.LineSegment2d ( _, end )) =
    end


{-| -}
endpoints : LineSegment2d -> ( Point2d, Point2d )
endpoints (Types.LineSegment2d endpoints_) =
    endpoints_


{-| -}
reverse : LineSegment2d -> LineSegment2d
reverse lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( p2, p1 )


{-| -}
midpoint : LineSegment2d -> Point2d
midpoint lineSegment =
    interpolate lineSegment 0.5


{-| -}
interpolate : LineSegment2d -> Float -> Point2d
interpolate lineSegment =
    let
        ( start, end ) =
            endpoints lineSegment
    in
    Point2d.interpolateFrom start end


{-| -}
length : LineSegment2d -> Float
length =
    vector >> Vector2d.length


{-| -}
squaredLength : LineSegment2d -> Float
squaredLength =
    vector >> Vector2d.squaredLength


{-| -}
direction : LineSegment2d -> Maybe Direction2d
direction =
    vector >> Vector2d.direction


{-| -}
perpendicularDirection : LineSegment2d -> Maybe Direction2d
perpendicularDirection =
    vector >> Vector2d.perpendicularTo >> Vector2d.direction


{-| -}
vector : LineSegment2d -> Vector2d
vector lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    Vector2d.from p1 p2


{-| -}
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


{-| -}
scaleAbout : Point2d -> Float -> LineSegment2d -> LineSegment2d
scaleAbout point scale =
    mapEndpoints (Point2d.scaleAbout point scale)


{-| -}
rotateAround : Point2d -> Float -> LineSegment2d -> LineSegment2d
rotateAround centerPoint angle =
    mapEndpoints (Point2d.rotateAround centerPoint angle)


{-| -}
translateBy : Vector2d -> LineSegment2d -> LineSegment2d
translateBy displacementVector =
    mapEndpoints (Point2d.translateBy displacementVector)


{-| -}
translateIn : Direction2d -> Float -> LineSegment2d -> LineSegment2d
translateIn translationDirection distance lineSegment =
    translateBy (Vector2d.withLength distance translationDirection) lineSegment


{-| -}
mirrorAcross : Axis2d -> LineSegment2d -> LineSegment2d
mirrorAcross axis =
    mapEndpoints (Point2d.mirrorAcross axis)


{-| -}
projectOnto : Axis2d -> LineSegment2d -> LineSegment2d
projectOnto axis =
    mapEndpoints (Point2d.projectOnto axis)


{-| -}
mapEndpoints : (Point2d -> Point2d) -> LineSegment2d -> LineSegment2d
mapEndpoints function lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( function p1, function p2 )


{-| -}
relativeTo : Frame2d -> LineSegment2d -> LineSegment2d
relativeTo frame =
    mapEndpoints (Point2d.relativeTo frame)


{-| -}
placeIn : Frame2d -> LineSegment2d -> LineSegment2d
placeIn frame =
    mapEndpoints (Point2d.placeIn frame)


{-| -}
boundingBox : LineSegment2d -> BoundingBox2d
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    BoundingBox2d.from p1 p2
