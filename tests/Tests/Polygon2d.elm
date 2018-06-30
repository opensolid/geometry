module Tests.Polygon2d exposing (..)

import Expect
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import LineSegment2d
import Polygon2d
import Test exposing (Test)
import Triangle2d
import TriangularMesh
import Vector2d


convexHullIsConvex : Test
convexHullIsConvex =
    Test.fuzz (Fuzz.list Fuzz.point2d)
        "The convex hull of a list of points is actually convex"
        (\points ->
            let
                convexHull =
                    Polygon2d.convexHull points

                edgeVectors =
                    Polygon2d.edges convexHull |> List.map LineSegment2d.vector
            in
            case edgeVectors of
                [] ->
                    Expect.pass

                [ single ] ->
                    Expect.fail "Polygon should never have a single edge"

                first :: rest ->
                    let
                        crossProducts =
                            List.map2 Vector2d.crossProduct
                                (first :: rest)
                                (rest ++ [ first ])

                        isNonNegative crossProduct =
                            crossProduct >= 0
                    in
                    Expect.true "Edges should always turn counterclockwise" <|
                        List.all isNonNegative crossProducts
        )


convexHullContainsAllPoints : Test
convexHullContainsAllPoints =
    Test.fuzz (Fuzz.list Fuzz.point2d)
        "The convex hull of a list of points contains all of those points"
        (\points ->
            let
                convexHull =
                    Polygon2d.convexHull points

                edges =
                    Polygon2d.edges convexHull

                isNonNegativeArea point edge =
                    let
                        ( p1, p2 ) =
                            LineSegment2d.endpoints edge

                        triangle =
                            Triangle2d.fromVertices ( point, p1, p2 )
                    in
                    Triangle2d.counterclockwiseArea triangle >= 0

                isContained point =
                    List.all (\edge -> isNonNegativeArea point edge) edges
            in
            Expect.true "Convex hull should contain all points" <|
                List.all isContained points
        )


triangulationHasCorrectArea : Test
triangulationHasCorrectArea =
    Test.fuzz Fuzz.polygon2d
        "The triangulation of a polygon has the same area as the polygon itself"
        (\polygon ->
            let
                polygonArea =
                    Polygon2d.area polygon

                triangles =
                    Polygon2d.triangulate polygon
                        |> TriangularMesh.faceVertices
                        |> List.map Triangle2d.fromVertices

                triangleArea =
                    List.sum (List.map Triangle2d.area triangles)
            in
            triangleArea |> Expect.approximately polygonArea
        )


triangulationHasCorrectNumberOfTriangles : Test
triangulationHasCorrectNumberOfTriangles =
    Test.fuzz Fuzz.polygon2d
        "The triangulation of a polygon with n vertices and h holes has n + 2h - 2 triangles"
        (\polygon ->
            let
                innerLoops =
                    Polygon2d.innerLoops polygon

                numberOfVertices =
                    List.length (Polygon2d.outerLoop polygon)
                        + List.sum (List.map List.length innerLoops)

                numberOfHoles =
                    List.length innerLoops

                expectedNumberOfTriangles =
                    numberOfVertices + 2 * numberOfHoles - 2
            in
            polygon
                |> Polygon2d.triangulate
                |> TriangularMesh.faceVertices
                |> List.length
                |> Expect.equal expectedNumberOfTriangles
        )
