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


module Polyline2d
    exposing
        ( Polyline2d
        , boundingBox
        , fromVertices
        , length
        , mapVertices
        , mirrorAcross
        , placeIn
        , projectOnto
        , relativeTo
        , rotateAround
        , scaleAbout
        , segments
        , translateBy
        , translateIn
        , vertices
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Polyline2d/icon.svg" alt="Polyline2d" width="160">

A `Polyline2d` represents a sequence of vertices in 2D connected by line
segments. This module contains a variety of polyline-related functionality, such
as

  - Computing the length of polylines
  - Scaling, rotating, translating and mirroring polylines
  - Converting polylines between different coordinate systems

@docs Polyline2d


# Constructors

@docs fromVertices


# Properties

@docs vertices, segments, length, boundingBox


# Transformations

Transforming a polyline is equivalent to transforming each of its vertices.

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapVertices


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


{-| -}
type alias Polyline2d =
    Types.Polyline2d


{-| -}
fromVertices : List Point2d -> Polyline2d
fromVertices =
    Types.Polyline2d


{-| -}
vertices : Polyline2d -> List Point2d
vertices (Types.Polyline2d vertices_) =
    vertices_


{-| -}
segments : Polyline2d -> List LineSegment2d
segments polyline =
    case vertices polyline of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 LineSegment2d.from all rest


{-| -}
length : Polyline2d -> Float
length =
    segments >> List.map LineSegment2d.length >> List.sum


{-| -}
scaleAbout : Point2d -> Float -> Polyline2d -> Polyline2d
scaleAbout point scale =
    mapVertices (Point2d.scaleAbout point scale)


{-| -}
rotateAround : Point2d -> Float -> Polyline2d -> Polyline2d
rotateAround point angle =
    mapVertices (Point2d.rotateAround point angle)


{-| -}
translateBy : Vector2d -> Polyline2d -> Polyline2d
translateBy vector =
    mapVertices (Point2d.translateBy vector)


{-| -}
translateIn : Direction2d -> Float -> Polyline2d -> Polyline2d
translateIn direction distance polyline =
    translateBy (Vector2d.withLength distance direction) polyline


{-| -}
mirrorAcross : Axis2d -> Polyline2d -> Polyline2d
mirrorAcross axis =
    mapVertices (Point2d.mirrorAcross axis)


{-| -}
projectOnto : Axis2d -> Polyline2d -> Polyline2d
projectOnto axis =
    mapVertices (Point2d.projectOnto axis)


{-| -}
mapVertices : (Point2d -> Point2d) -> Polyline2d -> Polyline2d
mapVertices function =
    vertices >> List.map function >> fromVertices


{-| -}
relativeTo : Frame2d -> Polyline2d -> Polyline2d
relativeTo frame =
    mapVertices (Point2d.relativeTo frame)


{-| -}
placeIn : Frame2d -> Polyline2d -> Polyline2d
placeIn frame =
    mapVertices (Point2d.placeIn frame)


{-| -}
boundingBox : Polyline2d -> Maybe BoundingBox2d
boundingBox polyline =
    BoundingBox2d.containingPoints (vertices polyline)
