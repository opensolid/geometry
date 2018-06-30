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


module Geometry.Expect
    exposing
        ( Comparison
        , angle
        , angleWithin
        , approximately
        , arc2d
        , arc3d
        , axis2d
        , axis3d
        , boundingBox2d
        , boundingBox2dWithin
        , boundingBox3d
        , boundingBox3dWithin
        , circle2d
        , circle3d
        , cubicSpline2d
        , cubicSpline3d
        , direction2d
        , direction2dWithin
        , direction3d
        , direction3dWithin
        , expect
        , frame2d
        , frame3d
        , just
        , lineSegment2d
        , lineSegment2dWithin
        , lineSegment3d
        , lineSegment3dWithin
        , maybe
        , plane3d
        , point2d
        , point2dWithin
        , point3d
        , point3dWithin
        , polygon2d
        , polygon2dWithin
        , polyline2d
        , polyline2dWithin
        , polyline3d
        , polyline3dWithin
        , quadraticSpline2d
        , quadraticSpline3d
        , sketchPlane3d
        , sphere3d
        , triangle2d
        , triangle2dWithin
        , triangle3d
        , triangle3dWithin
        , validDirection2d
        , validDirection3d
        , validFrame2d
        , validFrame3d
        , vector2d
        , vector2dWithin
        , vector3d
        , vector3dWithin
        )

import Arc2d exposing (Arc2d)
import Arc3d exposing (Arc3d)
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Expect exposing (Expectation)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Future.Debug as Debug
import Future.String as String
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Triangle2d exposing (Triangle2d)
import Triangle3d exposing (Triangle3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


type alias Comparison a =
    a -> a -> Bool


expect : Comparison a -> (a -> a -> Expectation)
expect comparison first second =
    if comparison first second then
        Expect.pass
    else
        let
            message =
                "Expected " ++ Debug.toString first ++ ", got " ++ Debug.toString second
        in
        Expect.fail message


by : Comparison b -> (a -> b) -> Comparison a
by comparison property first second =
    comparison (property first) (property second)


allOf : List (Comparison a) -> Comparison a
allOf comparisons first second =
    List.all (\comparison -> comparison first second) comparisons


listOf : Comparison a -> Comparison (List a)
listOf comparison firstList secondList =
    case ( firstList, secondList ) of
        ( [], [] ) ->
            True

        ( _, [] ) ->
            False

        ( [], _ ) ->
            False

        ( firstHead :: firstTail, secondHead :: secondTail ) ->
            comparison firstHead secondHead
                && listOf comparison firstTail secondTail


just : (expected -> actual -> Expectation) -> expected -> Maybe actual -> Expectation
just expect_ expectedValue actualMaybe =
    case actualMaybe of
        Just actualValue ->
            actualValue |> expect_ expectedValue

        Nothing ->
            Expect.fail "Expected a Just but got Nothing"


maybe : (expected -> actual -> Expectation) -> Maybe expected -> Maybe actual -> Expectation
maybe expect_ expectedMaybe actualMaybe =
    case ( expectedMaybe, actualMaybe ) of
        ( Just expectedValue, Just actualValue ) ->
            actualValue |> expect_ expectedValue

        ( Just _, Nothing ) ->
            Expect.fail "Expected a Just but got Nothing"

        ( Nothing, Just _ ) ->
            Expect.fail "Expected Nothing but got a Just"

        ( Nothing, Nothing ) ->
            Expect.pass


defaultTolerance : Float
defaultTolerance =
    1.0e-12


approximately : Float -> Float -> Expectation
approximately =
    Expect.within (Expect.AbsoluteOrRelative defaultTolerance defaultTolerance)


absoluteToleranceFor : Float -> Float
absoluteToleranceFor magnitude =
    max defaultTolerance (defaultTolerance * abs magnitude)


angle : Float -> Float -> Expectation
angle first second =
    angleWithin (absoluteToleranceFor (abs first)) first second


angleWithin : Float -> Float -> Float -> Expectation
angleWithin tolerance =
    let
        comparison firstAngle secondAngle =
            let
                difference =
                    secondAngle - firstAngle
            in
            abs (atan2 (sin difference) (cos difference)) <= tolerance
    in
    expect comparison


vector2d : Vector2d -> Vector2d -> Expectation
vector2d first second =
    vector2dWithin (absoluteToleranceFor (Vector2d.length first)) first second


vector2dWithin : Float -> Vector2d -> Vector2d -> Expectation
vector2dWithin tolerance =
    expect (Vector2d.equalWithin tolerance)


vector3d : Vector3d -> Vector3d -> Expectation
vector3d first second =
    vector3dWithin (absoluteToleranceFor (Vector3d.length first)) first second


vector3dWithin : Float -> Vector3d -> Vector3d -> Expectation
vector3dWithin tolerance =
    expect (Vector3d.equalWithin tolerance)


validDirection2d : Direction2d -> Expectation
validDirection2d direction =
    let
        length =
            Vector2d.length (Direction2d.toVector direction)
    in
    if abs (length - 1) <= defaultTolerance then
        Expect.pass
    else
        Expect.fail
            ("Expected "
                ++ Debug.toString direction
                ++ " to have length 1, but actually has length "
                ++ String.fromFloat length
            )


direction2d : Direction2d -> Direction2d -> Expectation
direction2d =
    direction2dWithin defaultTolerance


direction2dWithin : Float -> Direction2d -> Direction2d -> Expectation
direction2dWithin tolerance =
    expect (Direction2d.equalWithin tolerance)


validDirection3d : Direction3d -> Expectation
validDirection3d direction =
    let
        length =
            Vector3d.length (Direction3d.toVector direction)
    in
    if abs (length - 1) <= defaultTolerance then
        Expect.pass
    else
        Expect.fail
            ("Expected "
                ++ Debug.toString direction
                ++ " to have length 1, but actually has length "
                ++ String.fromFloat length
            )


direction3d : Direction3d -> Direction3d -> Expectation
direction3d =
    direction3dWithin defaultTolerance


direction3dWithin : Float -> Direction3d -> Direction3d -> Expectation
direction3dWithin tolerance =
    expect (Direction3d.equalWithin tolerance)


point2dTolerance : Point2d -> Float
point2dTolerance point =
    absoluteToleranceFor (Point2d.distanceFrom Point2d.origin point)


point2d : Point2d -> Point2d -> Expectation
point2d first second =
    point2dWithin (point2dTolerance first) first second


point2dWithin : Float -> Point2d -> Point2d -> Expectation
point2dWithin tolerance =
    expect (Point2d.equalWithin tolerance)


point3dTolerance : Point3d -> Float
point3dTolerance point =
    absoluteToleranceFor (Point3d.distanceFrom Point3d.origin point)


point3d : Point3d -> Point3d -> Expectation
point3d first second =
    point3dWithin (point3dTolerance first) first second


point3dWithin : Float -> Point3d -> Point3d -> Expectation
point3dWithin tolerance =
    expect (Point3d.equalWithin tolerance)


axis2d : Axis2d -> Axis2d -> Expectation
axis2d first =
    Expect.all
        [ Axis2d.originPoint >> point2d (Axis2d.originPoint first)
        , Axis2d.direction >> direction2d (Axis2d.direction first)
        ]


axis3d : Axis3d -> Axis3d -> Expectation
axis3d first =
    Expect.all
        [ Axis3d.originPoint >> point3d (Axis3d.originPoint first)
        , Axis3d.direction >> direction3d (Axis3d.direction first)
        ]


plane3d : Plane3d -> Plane3d -> Expectation
plane3d first =
    Expect.all
        [ Plane3d.originPoint >> point3d (Plane3d.originPoint first)
        , Plane3d.normalDirection >> direction3d (Plane3d.normalDirection first)
        ]


validFrame2d : Frame2d -> Expectation
validFrame2d =
    Expect.all
        [ Frame2d.xDirection >> validDirection2d
        , Frame2d.yDirection >> validDirection2d
        , \frame ->
            let
                xDirection =
                    Frame2d.xDirection frame

                yDirection =
                    Frame2d.yDirection frame

                parallelComponent =
                    Direction2d.componentIn xDirection yDirection
            in
            if abs parallelComponent <= defaultTolerance then
                Expect.pass
            else
                Expect.fail
                    ("Expected perpendicular basis directions, got "
                        ++ Debug.toString xDirection
                        ++ ", "
                        ++ Debug.toString yDirection
                    )
        ]


frame2d : Frame2d -> Frame2d -> Expectation
frame2d first =
    Expect.all
        [ Frame2d.originPoint >> point2d (Frame2d.originPoint first)
        , Frame2d.xDirection >> direction2d (Frame2d.xDirection first)
        , Frame2d.yDirection >> direction2d (Frame2d.yDirection first)
        ]


validFrame3d : Frame3d -> Expectation
validFrame3d =
    Expect.all
        [ Frame3d.xDirection >> validDirection3d
        , Frame3d.yDirection >> validDirection3d
        , Frame3d.zDirection >> validDirection3d
        , \frame ->
            let
                xDirection =
                    Frame3d.xDirection frame

                yDirection =
                    Frame3d.yDirection frame

                zDirection =
                    Frame3d.zDirection frame

                xyComponent =
                    Direction3d.componentIn xDirection yDirection

                yzComponent =
                    Direction3d.componentIn yDirection zDirection

                zxComponent =
                    Direction3d.componentIn zDirection xDirection
            in
            if
                (abs xyComponent <= defaultTolerance)
                    && (abs yzComponent <= defaultTolerance)
                    && (abs zxComponent <= defaultTolerance)
            then
                Expect.pass
            else
                Expect.fail
                    ("Expected perpendicular basis directions, got "
                        ++ Debug.toString xDirection
                        ++ ", "
                        ++ Debug.toString yDirection
                        ++ ", "
                        ++ Debug.toString zDirection
                    )
        ]


frame3d : Frame3d -> Frame3d -> Expectation
frame3d first =
    Expect.all
        [ Frame3d.originPoint >> point3d (Frame3d.originPoint first)
        , Frame3d.xDirection >> direction3d (Frame3d.xDirection first)
        , Frame3d.yDirection >> direction3d (Frame3d.yDirection first)
        , Frame3d.zDirection >> direction3d (Frame3d.zDirection first)
        ]


sketchPlane3d : SketchPlane3d -> SketchPlane3d -> Expectation
sketchPlane3d first =
    Expect.all
        [ SketchPlane3d.originPoint
            >> point3d (SketchPlane3d.originPoint first)
        , SketchPlane3d.xDirection
            >> direction3d (SketchPlane3d.xDirection first)
        , SketchPlane3d.yDirection
            >> direction3d (SketchPlane3d.yDirection first)
        ]


lineSegment2d : LineSegment2d -> LineSegment2d -> Expectation
lineSegment2d first =
    Expect.all
        [ LineSegment2d.startPoint
            >> point2d (LineSegment2d.startPoint first)
        , LineSegment2d.endPoint
            >> point2d (LineSegment2d.endPoint first)
        ]


lineSegment2dWithin : Float -> LineSegment2d -> LineSegment2d -> Expectation
lineSegment2dWithin tolerance first =
    Expect.all
        [ LineSegment2d.startPoint
            >> point2dWithin tolerance (LineSegment2d.startPoint first)
        , LineSegment2d.endPoint
            >> point2dWithin tolerance (LineSegment2d.endPoint first)
        ]


lineSegment3d : LineSegment3d -> LineSegment3d -> Expectation
lineSegment3d first =
    Expect.all
        [ LineSegment3d.startPoint
            >> point3d (LineSegment3d.startPoint first)
        , LineSegment3d.endPoint
            >> point3d (LineSegment3d.endPoint first)
        ]


lineSegment3dWithin : Float -> LineSegment3d -> LineSegment3d -> Expectation
lineSegment3dWithin tolerance first =
    Expect.all
        [ LineSegment3d.startPoint
            >> point3dWithin tolerance (LineSegment3d.startPoint first)
        , LineSegment3d.endPoint
            >> point3dWithin tolerance (LineSegment3d.endPoint first)
        ]


triangle2dBy : (Point2d -> Point2d -> Expectation) -> Triangle2d -> Triangle2d -> Expectation
triangle2dBy equalTo first =
    let
        vertex1 triangle =
            let
                ( vertex, _, _ ) =
                    Triangle2d.vertices triangle
            in
            vertex

        vertex2 triangle =
            let
                ( _, vertex, _ ) =
                    Triangle2d.vertices triangle
            in
            vertex

        vertex3 triangle =
            let
                ( _, _, vertex ) =
                    Triangle2d.vertices triangle
            in
            vertex
    in
    Expect.all
        [ vertex1 >> equalTo (vertex1 first)
        , vertex2 >> equalTo (vertex2 first)
        , vertex3 >> equalTo (vertex3 first)
        ]


triangle2d : Triangle2d -> Triangle2d -> Expectation
triangle2d =
    triangle2dBy point2d


triangle2dWithin : Float -> Triangle2d -> Triangle2d -> Expectation
triangle2dWithin tolerance =
    triangle2dBy (point2dWithin tolerance)


triangle3dBy : (Point3d -> Point3d -> Expectation) -> Triangle3d -> Triangle3d -> Expectation
triangle3dBy equalTo first =
    let
        vertex1 triangle =
            let
                ( vertex, _, _ ) =
                    Triangle3d.vertices triangle
            in
            vertex

        vertex2 triangle =
            let
                ( _, vertex, _ ) =
                    Triangle3d.vertices triangle
            in
            vertex

        vertex3 triangle =
            let
                ( _, _, vertex ) =
                    Triangle3d.vertices triangle
            in
            vertex
    in
    Expect.all
        [ vertex1 >> equalTo (vertex1 first)
        , vertex2 >> equalTo (vertex2 first)
        , vertex3 >> equalTo (vertex3 first)
        ]


triangle3d : Triangle3d -> Triangle3d -> Expectation
triangle3d =
    triangle3dBy point3d


triangle3dWithin : Float -> Triangle3d -> Triangle3d -> Expectation
triangle3dWithin tolerance =
    triangle3dBy (point3dWithin tolerance)


boundingBox2dBy : (Float -> Float -> Expectation) -> BoundingBox2d -> BoundingBox2d -> Expectation
boundingBox2dBy equalTo first =
    Expect.all
        [ BoundingBox2d.minX >> equalTo (BoundingBox2d.minX first)
        , BoundingBox2d.maxX >> equalTo (BoundingBox2d.maxX first)
        , BoundingBox2d.minY >> equalTo (BoundingBox2d.minY first)
        , BoundingBox2d.maxY >> equalTo (BoundingBox2d.maxY first)
        ]


boundingBox2d : BoundingBox2d -> BoundingBox2d -> Expectation
boundingBox2d =
    boundingBox2dBy approximately


boundingBox2dWithin : Float -> BoundingBox2d -> BoundingBox2d -> Expectation
boundingBox2dWithin tolerance =
    boundingBox2dBy (Expect.within (Expect.Absolute tolerance))


boundingBox3dBy : (Float -> Float -> Expectation) -> BoundingBox3d -> BoundingBox3d -> Expectation
boundingBox3dBy equalTo first =
    Expect.all
        [ BoundingBox3d.minX >> equalTo (BoundingBox3d.minX first)
        , BoundingBox3d.maxX >> equalTo (BoundingBox3d.maxX first)
        , BoundingBox3d.minY >> equalTo (BoundingBox3d.minY first)
        , BoundingBox3d.maxY >> equalTo (BoundingBox3d.maxY first)
        , BoundingBox3d.minZ >> equalTo (BoundingBox3d.minZ first)
        , BoundingBox3d.maxZ >> equalTo (BoundingBox3d.maxZ first)
        ]


boundingBox3d : BoundingBox3d -> BoundingBox3d -> Expectation
boundingBox3d =
    boundingBox3dBy approximately


boundingBox3dWithin : Float -> BoundingBox3d -> BoundingBox3d -> Expectation
boundingBox3dWithin tolerance =
    boundingBox3dBy (Expect.within (Expect.Absolute tolerance))


polyline2dBy : (Point2d -> Point2d -> Expectation) -> Polyline2d -> Polyline2d -> Expectation
polyline2dBy equalTo first =
    Expect.all
        [ Polyline2d.vertices
            >> List.length
            >> Expect.equal (List.length (Polyline2d.vertices first))
        , \second ->
            Expect.all
                (List.map2
                    (\firstVertex secondVertex ->
                        \_ -> secondVertex |> equalTo firstVertex
                    )
                    (Polyline2d.vertices first)
                    (Polyline2d.vertices second)
                )
                second
        ]


polyline2d : Polyline2d -> Polyline2d -> Expectation
polyline2d =
    polyline2dBy point2d


polyline2dWithin : Float -> Polyline2d -> Polyline2d -> Expectation
polyline2dWithin tolerance =
    polyline2dBy (point2dWithin tolerance)


polyline3dBy : (Point3d -> Point3d -> Expectation) -> Polyline3d -> Polyline3d -> Expectation
polyline3dBy equalPoints first =
    Expect.all
        [ Polyline3d.vertices
            >> List.length
            >> Expect.equal (List.length (Polyline3d.vertices first))
        , \second ->
            Expect.all
                (List.map2
                    (\firstVertex secondVertex ->
                        \_ -> equalPoints firstVertex secondVertex
                    )
                    (Polyline3d.vertices first)
                    (Polyline3d.vertices second)
                )
                second
        ]


polyline3d : Polyline3d -> Polyline3d -> Expectation
polyline3d =
    polyline3dBy point3d


polyline3dWithin : Float -> Polyline3d -> Polyline3d -> Expectation
polyline3dWithin tolerance =
    polyline3dBy (point3dWithin tolerance)


polygon2dBy : (Point2d -> Point2d -> Expectation) -> Polygon2d -> Polygon2d -> Expectation
polygon2dBy equalPoints first =
    Expect.all
        [ Polygon2d.vertices
            >> List.length
            >> Expect.equal (List.length (Polygon2d.vertices first))
        , \second ->
            Expect.all
                (List.map2
                    (\firstVertex secondVertex ->
                        \_ -> equalPoints firstVertex secondVertex
                    )
                    (Polygon2d.vertices first)
                    (Polygon2d.vertices second)
                )
                second
        ]


polygon2d : Polygon2d -> Polygon2d -> Expectation
polygon2d =
    polygon2dBy point2d


polygon2dWithin : Float -> Polygon2d -> Polygon2d -> Expectation
polygon2dWithin tolerance =
    polygon2dBy (point2dWithin tolerance)


circle2d : Circle2d -> Circle2d -> Expectation
circle2d first =
    Expect.all
        [ Circle2d.centerPoint >> point2d (Circle2d.centerPoint first)
        , Circle2d.radius >> approximately (Circle2d.radius first)
        ]


circle3d : Circle3d -> Circle3d -> Expectation
circle3d first =
    Expect.all
        [ Circle3d.centerPoint >> point3d (Circle3d.centerPoint first)
        , Circle3d.axialDirection >> direction3d (Circle3d.axialDirection first)
        , Circle3d.radius >> approximately (Circle3d.radius first)
        ]


sphere3d : Sphere3d -> Sphere3d -> Expectation
sphere3d first =
    Expect.all
        [ Sphere3d.centerPoint >> point3d (Sphere3d.centerPoint first)
        , Sphere3d.radius >> approximately (Sphere3d.radius first)
        ]


arc2d : Arc2d -> Arc2d -> Expectation
arc2d first =
    Expect.all
        [ Arc2d.startPoint >> point2d (Arc2d.startPoint first)
        , Arc2d.endPoint >> point2d (Arc2d.endPoint first)
        , Arc2d.sweptAngle >> approximately (Arc2d.sweptAngle first)
        ]


arc3d : Arc3d -> Arc3d -> Expectation
arc3d first =
    Expect.all
        [ Arc3d.startPoint >> point3d (Arc3d.startPoint first)
        , Arc3d.endPoint >> point3d (Arc3d.endPoint first)
        , Arc3d.sweptAngle >> approximately (Arc3d.sweptAngle first)
        , Arc3d.axialDirection >> direction3d (Arc3d.axialDirection first)
        ]


quadraticSpline2d : QuadraticSpline2d -> QuadraticSpline2d -> Expectation
quadraticSpline2d first =
    Expect.all
        [ QuadraticSpline2d.startPoint
            >> point2d (QuadraticSpline2d.startPoint first)
        , QuadraticSpline2d.controlPoint
            >> point2d (QuadraticSpline2d.controlPoint first)
        , QuadraticSpline2d.endPoint
            >> point2d (QuadraticSpline2d.endPoint first)
        ]


quadraticSpline3d : QuadraticSpline3d -> QuadraticSpline3d -> Expectation
quadraticSpline3d first =
    Expect.all
        [ QuadraticSpline3d.startPoint
            >> point3d (QuadraticSpline3d.startPoint first)
        , QuadraticSpline3d.controlPoint
            >> point3d (QuadraticSpline3d.controlPoint first)
        , QuadraticSpline3d.endPoint
            >> point3d (QuadraticSpline3d.endPoint first)
        ]


cubicSpline2d : CubicSpline2d -> CubicSpline2d -> Expectation
cubicSpline2d first =
    Expect.all
        [ CubicSpline2d.startPoint
            >> point2d (CubicSpline2d.startPoint first)
        , CubicSpline2d.startControlPoint
            >> point2d (CubicSpline2d.startControlPoint first)
        , CubicSpline2d.endControlPoint
            >> point2d (CubicSpline2d.endControlPoint first)
        , CubicSpline2d.endPoint
            >> point2d (CubicSpline2d.endPoint first)
        ]


cubicSpline3d : CubicSpline3d -> CubicSpline3d -> Expectation
cubicSpline3d first =
    Expect.all
        [ CubicSpline3d.startPoint
            >> point3d (CubicSpline3d.startPoint first)
        , CubicSpline3d.startControlPoint
            >> point3d (CubicSpline3d.startControlPoint first)
        , CubicSpline3d.endControlPoint
            >> point3d (CubicSpline3d.endControlPoint first)
        , CubicSpline3d.endPoint
            >> point3d (CubicSpline3d.endPoint first)
        ]
