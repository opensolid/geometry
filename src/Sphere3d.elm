module Sphere3d
    exposing
        ( Sphere3d
        , boundingBox
        , centerPoint
        , circumference
        , contains
        , diameter
        , mirrorAcross
        , placeIn
        , projectInto
        , projectOnto
        , radius
        , relativeTo
        , rotateAround
        , scaleAbout
        , surfaceArea
        , throughPoints
        , translateBy
        , translateIn
        , unit
        , volume
        , withRadius
        )

{-| <img src="https://ianmackenzie.github.io/elm-geometry/1.0.0/Sphere3d/icon.svg" alt="Sphere3d" width="160">

A `Sphere3d` is defined by its center point and radius. This module contains
functionality for:

  - Constructing spheres through points
  - Scaling, rotating and translating spheres
  - Extracting sphere properties like center point and volume

@docs Sphere3d


# Constants

@docs unit


# Constructors

@docs withRadius, throughPoints


# Properties

@docs centerPoint, radius, diameter, volume, surfaceArea, circumference, boundingBox


# Queries

@docs contains


# Transformations

@docs scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectInto


# Coordinate conversions

@docs relativeTo, placeIn

-}

import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types exposing (Sphere3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


{--Imports for verifying the examples:

    import Geometry.Examples.Sphere3d exposing (..)
    import Geometry.Examples.Expect as Expect
    import Axis3d
    import BoundingBox3d
    import Circle2d
    import Circle3d
    import Direction3d
    import Float.Extra as Float
    import Frame3d exposing (Frame3d)
    import Plane3d
    import Point2d
    import Point3d exposing (Point3d)
    import SketchPlane3d
    import Sphere3d
    import Vector3d exposing (Vector3d)
-}


{-| -}
type alias Sphere3d =
    Types.Sphere3d


{-| -}
unit : Sphere3d
unit =
    withRadius 1 Point3d.origin


{-| -}
withRadius : Float -> Point3d -> Sphere3d
withRadius radius_ centerPoint_ =
    Types.Sphere3d
        { radius = abs radius_
        , centerPoint = centerPoint_
        }


{-| -}
throughPoints : Point3d -> Point3d -> Point3d -> Point3d -> Maybe Sphere3d
throughPoints p1 p2 p3 p4 =
    Circle3d.throughPoints p1 p2 p3
        |> Maybe.andThen
            {-
               First three points define a circle.
               All points M on the normal to this circle through the circle's center are equidistant to p1, p2 and p3.
               Therefore: find this point M such that it is equidistant to p1, p2, p3 and p4,
               this will be the center of the sphere.
            -}
            (\circle ->
                let
                    normalAxis =
                        Circle3d.axis circle

                    r =
                        Circle3d.radius circle

                    x =
                        Point3d.distanceFromAxis normalAxis p4

                    y =
                        Point3d.signedDistanceAlong normalAxis p4
                in
                if y /= 0 then
                    let
                        d =
                            (r * r - x * x - y * y) / (-2 * y)
                    in
                    Just <|
                        withRadius (sqrt (r * r + d * d))
                            (Point3d.along normalAxis d)
                else
                    Nothing
            )


{-| -}
centerPoint : Sphere3d -> Point3d
centerPoint (Types.Sphere3d properties) =
    properties.centerPoint


{-| -}
radius : Sphere3d -> Float
radius (Types.Sphere3d properties) =
    properties.radius


{-| -}
diameter : Sphere3d -> Float
diameter sphere =
    2 * radius sphere


{-| -}
circumference : Sphere3d -> Float
circumference sphere =
    2 * pi * radius sphere


{-| -}
surfaceArea : Sphere3d -> Float
surfaceArea sphere =
    let
        r =
            radius sphere
    in
    4 * pi * r * r


{-| -}
volume : Sphere3d -> Float
volume sphere =
    let
        r =
            radius sphere
    in
    4 / 3 * pi * r * r * r


{-| -}
scaleAbout : Point3d -> Float -> Sphere3d -> Sphere3d
scaleAbout point scale sphere =
    withRadius (abs scale * radius sphere)
        (Point3d.scaleAbout point scale (centerPoint sphere))


{-| -}
rotateAround : Axis3d -> Float -> Sphere3d -> Sphere3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle
    in
    \sphere ->
        withRadius (radius sphere)
            (rotatePoint (centerPoint sphere))


{-| -}
translateBy : Vector3d -> Sphere3d -> Sphere3d
translateBy displacement sphere =
    withRadius (radius sphere)
        (Point3d.translateBy displacement (centerPoint sphere))


{-| -}
translateIn : Direction3d -> Float -> Sphere3d -> Sphere3d
translateIn direction distance sphere =
    translateBy (Vector3d.withLength distance direction) sphere


{-| -}
mirrorAcross : Plane3d -> Sphere3d -> Sphere3d
mirrorAcross plane sphere =
    withRadius (radius sphere)
        (Point3d.mirrorAcross plane (centerPoint sphere))


{-| -}
relativeTo : Frame3d -> Sphere3d -> Sphere3d
relativeTo frame sphere =
    withRadius (radius sphere)
        (Point3d.relativeTo frame (centerPoint sphere))


{-| -}
placeIn : Frame3d -> Sphere3d -> Sphere3d
placeIn frame sphere =
    withRadius (radius sphere)
        (Point3d.placeIn frame (centerPoint sphere))


{-| -}
boundingBox : Sphere3d -> BoundingBox3d
boundingBox sphere =
    let
        r =
            radius sphere

        ( cx, cy, cz ) =
            Point3d.coordinates (centerPoint sphere)
    in
    BoundingBox3d.fromExtrema
        { minX = cx - r
        , maxX = cx + r
        , minY = cy - r
        , maxY = cy + r
        , minZ = cz - r
        , maxZ = cz + r
        }


{-| -}
contains : Point3d -> Sphere3d -> Bool
contains point sphere =
    let
        r =
            radius sphere
    in
    Point3d.squaredDistanceFrom (centerPoint sphere) point <= r * r


{-| -}
projectOnto : Plane3d -> Sphere3d -> Circle3d
projectOnto plane sphere =
    Circle3d.withRadius (radius sphere)
        (Plane3d.normalDirection plane)
        (Point3d.projectOnto plane (centerPoint sphere))


{-| -}
projectInto : SketchPlane3d -> Sphere3d -> Circle2d
projectInto sketchPlane sphere =
    Circle2d.withRadius (radius sphere)
        (Point3d.projectInto sketchPlane (centerPoint sphere))
