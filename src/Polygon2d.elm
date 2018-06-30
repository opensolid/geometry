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


module Polygon2d
    exposing
        ( Polygon2d
        , area
        , boundingBox
        , convexHull
        , edges
        , innerLoops
        , mirrorAcross
        , outerLoop
        , perimeter
        , placeIn
        , relativeTo
        , rotateAround
        , scaleAbout
        , singleLoop
        , translateBy
        , translateIn
        , triangulate
        , vertices
        , with
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Polygon2d/icon.svg" alt="Polygon2d" width="160">

A `Polygon2d` represents a closed polygon in 2D, optionally with holes. It is
defined by an outer loop of vertices and a list of inner loops defining any
holes. This module contains a variety of polygon-related functionality, such as

  - Computing the perimeter and area of polygons
  - Scaling, rotating, translating and mirroring polygons
  - Converting polygons between different coordinate systems
  - Triangulating polygons

@docs Polygon2d


# Constructors

@docs singleLoop, with, convexHull


# Properties

@docs outerLoop, innerLoops, vertices, edges, perimeter, area, boundingBox


# Transformations

Transforming a polygon is equivalent to transforming each of its vertices.

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross


# Coordinate conversions

@docs relativeTo, placeIn


# Triangulation

@docs triangulate

-}

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d.Monotone as Monotone
import Triangle2d exposing (Triangle2d)
import TriangularMesh exposing (TriangularMesh)
import Vector2d exposing (Vector2d)


{-| -}
type alias Polygon2d =
    Types.Polygon2d


counterclockwiseArea : List Point2d -> Float
counterclockwiseArea vertices_ =
    case vertices_ of
        [] ->
            0

        [ single ] ->
            0

        [ first, second ] ->
            0

        first :: second :: rest ->
            let
                segmentArea start end =
                    Triangle2d.counterclockwiseArea
                        (Triangle2d.fromVertices ( first, start, end ))

                segmentAreas =
                    List.map2 segmentArea (second :: rest) rest
            in
            List.sum segmentAreas


makeOuterLoop : List Point2d -> List Point2d
makeOuterLoop vertices_ =
    if counterclockwiseArea vertices_ >= 0 then
        vertices_
    else
        List.reverse vertices_


makeInnerLoop : List Point2d -> List Point2d
makeInnerLoop vertices_ =
    if counterclockwiseArea vertices_ <= 0 then
        vertices_
    else
        List.reverse vertices_


{-| -}
singleLoop : List Point2d -> Polygon2d
singleLoop vertices_ =
    Types.Polygon2d
        { outerLoop = makeOuterLoop vertices_
        , innerLoops = []
        }


{-| -}
with : { outerLoop : List Point2d, innerLoops : List (List Point2d) } -> Polygon2d
with arguments =
    Types.Polygon2d
        { outerLoop = makeOuterLoop arguments.outerLoop
        , innerLoops = List.map makeInnerLoop arguments.innerLoops
        }


counterclockwiseAround : Point2d -> Point2d -> Point2d -> Bool
counterclockwiseAround origin a b =
    Vector2d.crossProduct (Vector2d.from origin a) (Vector2d.from origin b) >= 0


chainHelp : List Point2d -> List Point2d -> List Point2d
chainHelp acc list =
    case ( acc, list ) of
        ( r1 :: r2 :: rs, x :: xs ) ->
            if counterclockwiseAround r2 r1 x then
                chainHelp (r2 :: rs) (x :: xs)
            else
                chainHelp (x :: acc) xs

        ( _, x :: xs ) ->
            chainHelp (x :: acc) xs

        ( _, [] ) ->
            List.drop 1 acc


chain : List Point2d -> List Point2d
chain =
    chainHelp []


{-| -}
convexHull : List Point2d -> Polygon2d
convexHull points =
    -- See http://www.algorithmist.com/index.php/Monotone_Chain_Convex_Hull
    -- for a description of the algorithm.
    let
        sorted =
            points |> List.sortBy Point2d.coordinates

        lower =
            chain sorted

        upper =
            chain (List.reverse sorted)
    in
    singleLoop (lower ++ upper)


{-| -}
outerLoop : Polygon2d -> List Point2d
outerLoop (Types.Polygon2d polygon) =
    polygon.outerLoop


{-| -}
innerLoops : Polygon2d -> List (List Point2d)
innerLoops (Types.Polygon2d polygon) =
    polygon.innerLoops


{-| -}
vertices : Polygon2d -> List Point2d
vertices polygon =
    List.concat (outerLoop polygon :: innerLoops polygon)


loopEdges : List Point2d -> List LineSegment2d
loopEdges vertices_ =
    case vertices_ of
        [] ->
            []

        (first :: rest) as all ->
            List.map2 LineSegment2d.from all (rest ++ [ first ])


{-| -}
edges : Polygon2d -> List LineSegment2d
edges polygon =
    let
        outerEdges =
            loopEdges (outerLoop polygon)

        innerEdges =
            List.map loopEdges (innerLoops polygon)
    in
    List.concat (outerEdges :: innerEdges)


{-| -}
perimeter : Polygon2d -> Float
perimeter =
    edges >> List.map LineSegment2d.length >> List.sum


{-| -}
area : Polygon2d -> Float
area polygon =
    counterclockwiseArea (outerLoop polygon)
        + List.sum (List.map counterclockwiseArea (innerLoops polygon))


{-| -}
scaleAbout : Point2d -> Float -> Polygon2d -> Polygon2d
scaleAbout point scale =
    mapVertices (Point2d.scaleAbout point scale) (scale < 0)


{-| -}
rotateAround : Point2d -> Float -> Polygon2d -> Polygon2d
rotateAround point angle =
    mapVertices (Point2d.rotateAround point angle) False


{-| -}
translateBy : Vector2d -> Polygon2d -> Polygon2d
translateBy vector =
    mapVertices (Point2d.translateBy vector) False


{-| -}
translateIn : Direction2d -> Float -> Polygon2d -> Polygon2d
translateIn direction distance polygon =
    translateBy (Vector2d.withLength distance direction) polygon


{-| -}
mirrorAcross : Axis2d -> Polygon2d -> Polygon2d
mirrorAcross axis =
    mapVertices (Point2d.mirrorAcross axis) True


mapVertices : (Point2d -> Point2d) -> Bool -> Polygon2d -> Polygon2d
mapVertices function invert polygon =
    let
        mappedOuterLoop =
            List.map function (outerLoop polygon)

        mappedInnerLoops =
            List.map (List.map function) (innerLoops polygon)
    in
    if invert then
        Types.Polygon2d
            { outerLoop = List.reverse mappedOuterLoop
            , innerLoops = List.map List.reverse mappedInnerLoops
            }
    else
        Types.Polygon2d
            { outerLoop = mappedOuterLoop
            , innerLoops = mappedInnerLoops
            }


{-| -}
relativeTo : Frame2d -> Polygon2d -> Polygon2d
relativeTo frame =
    mapVertices (Point2d.relativeTo frame) (not (Frame2d.isRightHanded frame))


{-| -}
placeIn : Frame2d -> Polygon2d -> Polygon2d
placeIn frame =
    mapVertices (Point2d.placeIn frame) (not (Frame2d.isRightHanded frame))


{-| -}
boundingBox : Polygon2d -> Maybe BoundingBox2d
boundingBox polygon =
    BoundingBox2d.containingPoints (outerLoop polygon)


{-| -}
triangulate : Polygon2d -> TriangularMesh Point2d
triangulate polygon =
    Monotone.triangulation polygon
