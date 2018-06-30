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


module Polyline3d
    exposing
        ( Polyline3d
        , boundingBox
        , fromVertices
        , length
        , mapVertices
        , mirrorAcross
        , on
        , placeIn
        , projectInto
        , projectOnto
        , relativeTo
        , rotateAround
        , scaleAbout
        , segments
        , translateBy
        , translateIn
        , vertices
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Polyline3d/icon.svg" alt="Polyline3d" width="160">

A `Polyline3d` represents a sequence of vertices in 3D connected by line
segments. This module contains a variety of polyline-related functionality, such
as

  - Computing the length of polylines
  - Scaling, rotating, translating and mirroring polylines
  - Converting polylines between different coordinate systems

@docs Polyline3d


# Constructors

@docs fromVertices, on


# Properties

@docs vertices, segments, length, boundingBox


# Transformations

Transforming a polyline is equivalent to transforming each of its vertices.

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, mapVertices


# Coordinate conversions

@docs relativeTo, placeIn, projectInto

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline2d exposing (Polyline2d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Polyline3d =
    Types.Polyline3d


{-| -}
fromVertices : List Point3d -> Polyline3d
fromVertices =
    Types.Polyline3d


{-| -}
on : SketchPlane3d -> Polyline2d -> Polyline3d
on sketchPlane =
    Polyline2d.vertices >> List.map (Point3d.on sketchPlane) >> fromVertices


{-| -}
vertices : Polyline3d -> List Point3d
vertices (Types.Polyline3d vertices_) =
    vertices_


{-| -}
segments : Polyline3d -> List LineSegment3d
segments polyline =
    case vertices polyline of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 LineSegment3d.from all rest


{-| -}
length : Polyline3d -> Float
length =
    segments >> List.map LineSegment3d.length >> List.sum


{-| -}
scaleAbout : Point3d -> Float -> Polyline3d -> Polyline3d
scaleAbout point scale =
    mapVertices (Point3d.scaleAbout point scale)


{-| -}
rotateAround : Axis3d -> Float -> Polyline3d -> Polyline3d
rotateAround axis angle =
    mapVertices (Point3d.rotateAround axis angle)


{-| -}
translateBy : Vector3d -> Polyline3d -> Polyline3d
translateBy vector =
    mapVertices (Point3d.translateBy vector)


{-| -}
translateIn : Direction3d -> Float -> Polyline3d -> Polyline3d
translateIn direction distance polyline =
    translateBy (Vector3d.withLength distance direction) polyline


{-| -}
mirrorAcross : Plane3d -> Polyline3d -> Polyline3d
mirrorAcross plane =
    mapVertices (Point3d.mirrorAcross plane)


{-| -}
projectOnto : Plane3d -> Polyline3d -> Polyline3d
projectOnto plane =
    mapVertices (Point3d.projectOnto plane)


{-| -}
mapVertices : (Point3d -> Point3d) -> Polyline3d -> Polyline3d
mapVertices function =
    vertices >> List.map function >> fromVertices


{-| -}
relativeTo : Frame3d -> Polyline3d -> Polyline3d
relativeTo frame =
    mapVertices (Point3d.relativeTo frame)


{-| -}
placeIn : Frame3d -> Polyline3d -> Polyline3d
placeIn frame =
    mapVertices (Point3d.placeIn frame)


{-| -}
projectInto : SketchPlane3d -> Polyline3d -> Polyline2d
projectInto sketchPlane =
    vertices
        >> List.map (Point3d.projectInto sketchPlane)
        >> Polyline2d.fromVertices


{-| -}
boundingBox : Polyline3d -> Maybe BoundingBox3d
boundingBox polyline =
    BoundingBox3d.containingPoints (vertices polyline)
