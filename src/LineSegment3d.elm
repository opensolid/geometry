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


module LineSegment3d
    exposing
        ( LineSegment3d
        , along
        , boundingBox
        , direction
        , endPoint
        , endpoints
        , from
        , fromEndpoints
        , interpolate
        , length
        , mapEndpoints
        , midpoint
        , mirrorAcross
        , on
        , perpendicularDirection
        , placeIn
        , projectInto
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

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/LineSegment3d/icon.svg" alt="LineSegment3d" width="160">

A `LineSegment3d` is a line between two points in 3D. This module contains
functionality such as:

  - Interpolating within a line segment or measuring its length
  - Scaling, rotating, translating, mirroring or projecting a line segment
  - Converting a line segment between local and global coordinates in different
    reference frames

@docs LineSegment3d


# Constructors

@docs fromEndpoints, from, along, on


# Properties

@docs startPoint, endPoint, endpoints, midpoint, length, squaredLength, direction, perpendicularDirection, vector, boundingBox


# Interpolation

@docs interpolate


# Transformations

@docs reverse, scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapEndpoints


# Coordinate conversions

@docs relativeTo, placeIn, projectInto

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias LineSegment3d =
    Types.LineSegment3d


{-| -}
fromEndpoints : ( Point3d, Point3d ) -> LineSegment3d
fromEndpoints =
    Types.LineSegment3d


{-| -}
from : Point3d -> Point3d -> LineSegment3d
from startPoint_ endPoint_ =
    fromEndpoints ( startPoint_, endPoint_ )


{-| -}
along : Axis3d -> Float -> Float -> LineSegment3d
along axis start end =
    fromEndpoints ( Point3d.along axis start, Point3d.along axis end )


{-| -}
on : SketchPlane3d -> LineSegment2d -> LineSegment3d
on sketchPlane lineSegment2d =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints lineSegment2d
    in
    fromEndpoints
        ( Point3d.on sketchPlane p1
        , Point3d.on sketchPlane p2
        )


{-| -}
startPoint : LineSegment3d -> Point3d
startPoint (Types.LineSegment3d ( start, _ )) =
    start


{-| -}
endPoint : LineSegment3d -> Point3d
endPoint (Types.LineSegment3d ( _, end )) =
    end


{-| -}
endpoints : LineSegment3d -> ( Point3d, Point3d )
endpoints (Types.LineSegment3d endpoints_) =
    endpoints_


{-| -}
reverse : LineSegment3d -> LineSegment3d
reverse lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( p2, p1 )


{-| -}
midpoint : LineSegment3d -> Point3d
midpoint lineSegment =
    interpolate lineSegment 0.5


{-| -}
interpolate : LineSegment3d -> Float -> Point3d
interpolate lineSegment =
    let
        ( start, end ) =
            endpoints lineSegment
    in
    Point3d.interpolateFrom start end


{-| -}
length : LineSegment3d -> Float
length =
    vector >> Vector3d.length


{-| -}
squaredLength : LineSegment3d -> Float
squaredLength =
    vector >> Vector3d.squaredLength


{-| -}
direction : LineSegment3d -> Maybe Direction3d
direction =
    vector >> Vector3d.direction


{-| -}
perpendicularDirection : LineSegment3d -> Maybe Direction3d
perpendicularDirection =
    vector >> Vector3d.perpendicularTo >> Vector3d.direction


{-| -}
vector : LineSegment3d -> Vector3d
vector lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    Vector3d.from p1 p2


{-| -}
scaleAbout : Point3d -> Float -> LineSegment3d -> LineSegment3d
scaleAbout point scale =
    mapEndpoints (Point3d.scaleAbout point scale)


{-| -}
rotateAround : Axis3d -> Float -> LineSegment3d -> LineSegment3d
rotateAround axis angle =
    mapEndpoints (Point3d.rotateAround axis angle)


{-| -}
translateBy : Vector3d -> LineSegment3d -> LineSegment3d
translateBy displacementVector =
    mapEndpoints (Point3d.translateBy displacementVector)


{-| -}
translateIn : Direction3d -> Float -> LineSegment3d -> LineSegment3d
translateIn translationDirection distance lineSegment =
    translateBy (Vector3d.withLength distance translationDirection) lineSegment


{-| -}
mirrorAcross : Plane3d -> LineSegment3d -> LineSegment3d
mirrorAcross plane =
    mapEndpoints (Point3d.mirrorAcross plane)


{-| -}
projectOnto : Plane3d -> LineSegment3d -> LineSegment3d
projectOnto plane =
    mapEndpoints (Point3d.projectOnto plane)


{-| -}
mapEndpoints : (Point3d -> Point3d) -> LineSegment3d -> LineSegment3d
mapEndpoints function lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    fromEndpoints ( function p1, function p2 )


{-| -}
relativeTo : Frame3d -> LineSegment3d -> LineSegment3d
relativeTo frame =
    mapEndpoints (Point3d.relativeTo frame)


{-| -}
placeIn : Frame3d -> LineSegment3d -> LineSegment3d
placeIn frame =
    mapEndpoints (Point3d.placeIn frame)


{-| -}
projectInto : SketchPlane3d -> LineSegment3d -> LineSegment2d
projectInto sketchPlane lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment

        project =
            Point3d.projectInto sketchPlane
    in
    LineSegment2d.fromEndpoints ( project p1, project p2 )


{-| -}
boundingBox : LineSegment3d -> BoundingBox3d
boundingBox lineSegment =
    let
        ( p1, p2 ) =
            endpoints lineSegment
    in
    BoundingBox3d.from p1 p2
