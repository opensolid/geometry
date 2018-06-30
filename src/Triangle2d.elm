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


module Triangle2d
    exposing
        ( Triangle2d
        , area
        , boundingBox
        , centroid
        , circumcircle
        , clockwiseArea
        , contains
        , counterclockwiseArea
        , edges
        , fromVertices
        , mapVertices
        , mirrorAcross
        , placeIn
        , relativeTo
        , rotateAround
        , scaleAbout
        , translateBy
        , translateIn
        , vertices
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Triangle2d/icon.svg" alt="Triangle2d" width="160">

A `Triangle2d` represents a triangle in 2D space, and is defined by its three
vertices. This module contains triangle-related functionality such as:

  - Finding the area and centroid of triangles
  - Scaling, rotating, translating and mirroring triangles
  - Converting triangles between different coordinate systems

@docs Triangle2d


# Constructors

@docs fromVertices


# Properties

@docs vertices, edges, centroid, area, counterclockwiseArea, clockwiseArea, boundingBox, circumcircle


# Queries

@docs contains


# Transformations

Transforming a triangle is equivalent to transforming its vertices.

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, mapVertices


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d exposing (Circle2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Triangle2d =
    Types.Triangle2d


{-| -}
fromVertices : ( Point2d, Point2d, Point2d ) -> Triangle2d
fromVertices =
    Types.Triangle2d


{-| -}
vertices : Triangle2d -> ( Point2d, Point2d, Point2d )
vertices (Types.Triangle2d vertices_) =
    vertices_


{-| -}
edges : Triangle2d -> ( LineSegment2d, LineSegment2d, LineSegment2d )
edges triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    ( LineSegment2d.fromEndpoints ( p1, p2 )
    , LineSegment2d.fromEndpoints ( p2, p3 )
    , LineSegment2d.fromEndpoints ( p3, p1 )
    )


{-| -}
centroid : Triangle2d -> Point2d
centroid triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Vector2d.from p1 p2

        secondVector =
            Vector2d.from p1 p3

        displacement =
            Vector2d.scaleBy (1.0 / 3.0) (Vector2d.sum firstVector secondVector)
    in
    Point2d.translateBy displacement p1


{-| -}
contains : Point2d -> Triangle2d -> Bool
contains point triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        crossProduct startVertex endVertex =
            let
                vectorToPoint =
                    Vector2d.from startVertex point

                segmentVector =
                    Vector2d.from startVertex endVertex
            in
            Vector2d.crossProduct segmentVector vectorToPoint

        firstProduct =
            crossProduct p1 p2

        secondProduct =
            crossProduct p2 p3

        thirdProduct =
            crossProduct p3 p1
    in
    (firstProduct >= 0 && secondProduct >= 0 && thirdProduct >= 0)
        || (firstProduct <= 0 && secondProduct <= 0 && thirdProduct <= 0)


{-| -}
area : Triangle2d -> Float
area =
    counterclockwiseArea >> abs


{-| -}
counterclockwiseArea : Triangle2d -> Float
counterclockwiseArea triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Vector2d.from p1 p2

        secondVector =
            Vector2d.from p1 p3
    in
    0.5 * Vector2d.crossProduct firstVector secondVector


{-| -}
clockwiseArea : Triangle2d -> Float
clockwiseArea triangle =
    -(counterclockwiseArea triangle)


{-| -}
scaleAbout : Point2d -> Float -> Triangle2d -> Triangle2d
scaleAbout point scale =
    mapVertices (Point2d.scaleAbout point scale)


{-| -}
rotateAround : Point2d -> Float -> Triangle2d -> Triangle2d
rotateAround centerPoint angle =
    mapVertices (Point2d.rotateAround centerPoint angle)


{-| -}
translateBy : Vector2d -> Triangle2d -> Triangle2d
translateBy vector =
    mapVertices (Point2d.translateBy vector)


{-| -}
translateIn : Direction2d -> Float -> Triangle2d -> Triangle2d
translateIn direction distance triangle =
    translateBy (Vector2d.withLength distance direction) triangle


{-| -}
mirrorAcross : Axis2d -> Triangle2d -> Triangle2d
mirrorAcross axis =
    mapVertices (Point2d.mirrorAcross axis)


{-| -}
mapVertices : (Point2d -> Point2d) -> Triangle2d -> Triangle2d
mapVertices function triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    fromVertices ( function p1, function p2, function p3 )


{-| -}
relativeTo : Frame2d -> Triangle2d -> Triangle2d
relativeTo frame =
    mapVertices (Point2d.relativeTo frame)


{-| -}
placeIn : Frame2d -> Triangle2d -> Triangle2d
placeIn frame =
    mapVertices (Point2d.placeIn frame)


{-| -}
boundingBox : Triangle2d -> BoundingBox2d
boundingBox triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        ( x1, y1 ) =
            Point2d.coordinates p1

        ( x2, y2 ) =
            Point2d.coordinates p2

        ( x3, y3 ) =
            Point2d.coordinates p3
    in
    BoundingBox2d.fromExtrema
        { minX = min x1 (min x2 x3)
        , maxX = max x1 (max x2 x3)
        , minY = min y1 (min y2 y3)
        , maxY = max y1 (max y2 y3)
        }


{-| -}
circumcircle : Triangle2d -> Maybe Circle2d
circumcircle triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
    Circle2d.throughPoints p1 p2 p3
